#![no_std]

//! This is a low-overhead profiler based on the design presented by Casey Muratori in 2023 in his
//! [Computer, Enhance!](https://www.computerenhance.com/) programming educational series.
//!
//! The profiler measures inclusive and exclusive time (or CPU timestamp counters) of each profiled
//! zone, as well as the number of times the zone was entered (hit count). It does not do call
//! attribution, because that would require more heavyweight machinery per profile zone.
//!
//! Simple usage looks like this:
//!
//! ```
//! use core::ptr;
//! use wfprof::{profile_zone, Profiler};
//! // In this case, we use the shared profiler defined in the profiler crate itself, but we could
//! // also have used an instance we define ourselves.
//! use wfprof::PROFILER;
//!
//! fn fib(n: u64) -> u64 {
//!     // Inserting the profile_zone macro at the top of the function profiles
//!     // the entire function. We can also scope the macro to a block to profile
//!     // just that block.
//!     profile_zone!("fib");
//!
//!     if n <= 1 {
//!         return 1;
//!     }
//!
//!     add(fib(n - 1), fib(n - 2))
//! }
//!
//! fn add(a: u64, b: u64) -> u64 {
//!     profile_zone!("add");
//!     a + b
//! }
//!
//! fn main() {
//!     unsafe {
//!         Profiler::start(&raw mut PROFILER);
//!     }
//!
//!     // Run a function we want to profile.
//!     let fib5 = fib(5);
//!
//!     unsafe {
//!         Profiler::end(&raw mut PROFILER, "MAIN");
//!     }
//!
//!     println!("fib(5) = {fib5}");
//!     println!();
//!
//!     // Read zone data from profiler and print them out.
//!     //
//!     // SAFETY: Even if it would be okay to create exactly one live &mut to PROFILER
//!     // after profiling is done, we instead copy out data we need from the profiler.
//!     let profiler_zones = unsafe { PROFILER.zones };
//!
//!     println!("Printing profiling results:");
//!     println!();
//!
//!     let tsc_elapsed = profiler_zones[0].tsc_elapsed_incl;
//!
//!     for zone in &profiler_zones {
//!         if zone.label != "" {
//!             println!(
//!                 "{:<20} | {:>15} Hits | {:>15.9} Exclusive TSCs ({:>3.2}%) | {:>15.9} Inclusive TSCs ({:>3.2}%)",
//!                 zone.label,
//!                 zone.hit_count,
//!                 zone.tsc_elapsed_excl,
//!                 100.0 * zone.tsc_elapsed_excl as f64 / tsc_elapsed as f64,
//!                 zone.tsc_elapsed_incl,
//!                 100.0 * zone.tsc_elapsed_incl as f64 / tsc_elapsed as f64,
//!             );
//!         }
//!     }
//! }
//! ```
//!
//! Look at `examples/prof_usage.rs` for a more detailed usage.
//!
//! Technically, the differences between this profiler and the one presented by Casey are tiny, and
//! mostly stemming from the fact that this is Rust and not C. Otherwise the tracking of exclusive
//! and inclusive times works exactly as in Casey's design.
//!
//! Implementation differences:
//!
//! Casey builds in a single translation unit, and can thus utilize the __COUNTER preprocessor macro
//! to generate indices for the profile zones. Not only doesn't Rust have unity build, it doesn't
//! have __COUNTER either, nor any other simple way of getting a sequence of integers at compile
//! time.
//!
//! For this reason, this profiler hashes the zone label at compile time to generate the zone index
//! and uses that to access the global array of profile zones. At runtime, the profiler performs a
//! branchless conflict check when writing zone data, setting a flag if it detects a conflict.
//!
//! This adds minor runtime overhead: one more read, write, and a couple of bit operations, as well
//! as the less obvious but probably still observable overhead of having the profiling zones
//! scattered in the array, disabling any cache line reuse. Still, this is not too bad hopefully.

use wfcommon::static_assert;

// If you encounter a profiling zone conflict, double this number and check it into source control.
/// The number of zones in the [`Profiler`].
pub const PROFILER_ZONE_COUNT: usize = 4096;

/// A global [`Profiler`], by default targetted by the [`profile_zone`] macro.
///
/// # Warning
///
/// This is a mutable static without any synchronization. Do not makes mutable references to it. Do
/// not access it from multiple threads.
pub static mut PROFILER: Profiler = Profiler::new();

/// The Profiler is the place where all the profiling information lives.
///
/// The [`profile_zone`] macro assumes there is a `static mut` called PROFILER in scope, and
/// attempts to attach itself to it.
///
/// After all the measurements are taken, the [`Profiler::zones`] field contains the timing
/// information. Zones with no label (`""`) were not used, and contain no useful information.
///
/// Due to metaprogramming limitations, the profiler also has the [`Profiler::zones_conflicting`]
/// field. If the value is true after the profiling has finished, the hash function that generates
/// the zone indices generated the same index for two distinct zones, corrupting the profiling
/// results. If this happened to you, try increasing the [`PROFILER_ZONE_COUNT`] constant to lessen the
/// chances of hash conflicts happening. Unfortunately, also due to metaprogramming limitations,
/// there's no tasteful way of changing the zone count without modifying it in this source file.
// Aligned to page boundary, so that no profile zone straddles pages or cache lines.
#[repr(C, align(4096))]
pub struct Profiler {
    pub zones: [Zone; PROFILER_ZONE_COUNT],
    pub zone0_tsc_start: u64,
    pub zone_idx: usize,
    pub zones_conflicting: bool,
}

impl Profiler {
    pub const fn new() -> Self {
        Self {
            zones: [Zone {
                tsc_elapsed_incl: 0,
                tsc_elapsed_excl: 0,
                hit_count: 0,
                label: "",
            }; PROFILER_ZONE_COUNT],
            zone0_tsc_start: 0,
            zone_idx: 0,
            zones_conflicting: false,
        }
    }

    /// Measure the start time of profiling.
    ///
    /// This takes a raw pointer to make it easier to use correctly, without taking a mutable
    /// reference:
    ///
    /// ```
    /// use core::ptr;
    /// use wfprof::Profiler;
    ///
    /// static mut PROFILER: Profiler = Profiler::new();
    /// unsafe {
    ///     Profiler::start(&raw mut PROFILER);
    /// }
    /// ```
    ///
    /// # Safety
    ///
    /// The `profiler` parameter must point to a valid [`Profiler`].
    #[inline]
    pub unsafe fn start(profiler: *mut Self) {
        unsafe {
            (*profiler).zone0_tsc_start = read_cpu_timestamp_counter();
        }
    }

    /// Measure the end time of profiling and aggregate data for the topmost zone.
    ///
    /// This takes a raw pointer to make it easier to use correctly, without taking a mutable
    /// reference:
    ///
    /// ```
    /// use core::ptr;
    /// use wfprof::Profiler;
    ///
    /// static mut PROFILER: Profiler = Profiler::new();
    /// unsafe {
    ///     Profiler::end(&raw mut PROFILER, "MAIN_ZONE_NAME");
    /// }
    /// ```
    ///
    /// # Safety
    ///
    /// The `profiler` parameter must point to a valid [`Profiler`].
    #[inline]
    pub unsafe fn end(profiler: *mut Self, zone_label: &'static str) {
        let tsc_end = read_cpu_timestamp_counter();

        unsafe {
            let tsc_elapsed = tsc_end - (*profiler).zone0_tsc_start;

            let zone0 = slice_get_mut(&raw mut (*profiler).zones, 0);

            (*zone0).tsc_elapsed_incl = tsc_elapsed;
            (*zone0).tsc_elapsed_excl = (*zone0).tsc_elapsed_excl.wrapping_add(tsc_elapsed);
            (*zone0).hit_count = (*zone0).hit_count.wrapping_add(1);
            (*zone0).label = zone_label;
        }
    }

    /// Reset internal profiler data, so it can be used again.
    ///
    /// # Safety
    ///
    /// The `profiler` parameter must point to a valid [`Profiler`].
    #[inline]
    pub unsafe fn reset(profiler: *mut Self) {
        unsafe {
            (*profiler).zones = [Zone {
                tsc_elapsed_incl: 0,
                tsc_elapsed_excl: 0,
                hit_count: 0,
                label: "",
            }; PROFILER_ZONE_COUNT];
            (*profiler).zone0_tsc_start = 0;
            (*profiler).zone_idx = 0;
            (*profiler).zones_conflicting = false;
        }
    }
}

/// A profiling zone accumulating timing metrics for a piece of code.
///
/// It counts both inclusive and exclusive time, as well as the number of hits. The label is the
/// same label that's provided to the [`profile_zone`] macro.
#[repr(C, align(64))]
#[derive(Clone, Copy)]
pub struct Zone {
    pub tsc_elapsed_incl: u64,
    pub tsc_elapsed_excl: u64,
    pub hit_count: u64,
    pub label: &'static str,
}

static_assert!(size_of::<Zone>() == 64);
static_assert!(align_of::<Zone>() == 64);

/// This macro times a block of code.
///
/// It measures the elapsed time between the invocation of the macro up to the end of the block's
/// scope. After timing your program, the resulting times can be found in the [`Profiler`].
///
/// By default, the macro targets [`PROFILER`] defined in this module, but this can be overriden
/// with a second parameter, if your usecase needs to have multiple profilers, (e.g. because of a
/// threading situation).
///
/// # Warning
///
/// The [`Profiler`] this talks to is declared as a global mutable variable (`static mut`). Do not
/// invoke this macro from multiple threads, unless their profiler arguments are different. There is
/// no synchronization.
///
/// The profiler code attempts its best to not violate other rules of mutable statics, such as not
/// producing a `&mut` to the global anywhere, as the lifetime of those could easily overlap, and
/// that's considered Undefined Behavior.
///
/// [Nomicon on Aliasing](https://doc.rust-lang.org/nomicon/aliasing.html)
#[macro_export]
macro_rules! profile_zone {
    ($zone_label:expr) => {
        $crate::profile_zone!($zone_label, $crate::PROFILER)
    };
    ($zone_label:expr, $profiler:expr) => {
        //
        // Generate a zone index at compile time.
        //
        let zone_idx = {
            // It looks like consts don't have hygiene, so we have to enclose it in a block,
            // otherwise it would clash with constants defined in other invocations of this macro.
            const ZONE_IDX: usize = $crate::hash_zone_label($zone_label);
            ZONE_IDX
        };

        // SAFETY: be careful not to take a mut reference here, as multiple nested profiling blocks
        // can live at the same time, and we mustn's have multiple mutable references pointing to
        // the same memory, otherwise LLVM will eat our code.
        let prof = unsafe { &raw mut $profiler };

        //
        // Set this zone as the current zone, but remember our parent first, so that we can modify
        // its exclusive time later.
        //
        let parent_idx = unsafe { $profiler.zone_idx };
        unsafe {
            $profiler.zone_idx = zone_idx;
        }

        //
        // Remember the inclusive time of our zone. We'll use this to overwrite inclusive values
        // written by recursive calls to our zone.
        //
        // SAFETY: Avoid using the Index trait (which would use a reference and maybe anger LLVM
        // dragons?), and instead get the base pointer of the array and add to it. The index
        // *should* produce a pointer that is in the same "allocated object", as pointer::add
        // requires.
        //
        let zone = unsafe { $crate::slice_get(&raw const $profiler.zones, zone_idx) };
        let tsc_elapsed_incl = unsafe { (*zone).tsc_elapsed_incl };

        //
        // Time the beginning of the block.
        //
        let tsc_start = $crate::read_cpu_timestamp_counter();

        let _block = $crate::Block {
            tsc_start,
            tsc_elapsed_incl,
            zone_idx,
            zone_label: $zone_label,
            parent_idx,
            prof,
        };
    };
}

#[cfg(target_arch = "x86_64")]
#[inline(always)]
/// Read the x64 CPU's timestamp counter using the RDTSC instruction
/// ([`core::arch::x86_64::_rdtsc()`]).
pub fn read_cpu_timestamp_counter() -> u64 {
    unsafe { core::arch::x86_64::_rdtsc() }
}

#[cfg(target_arch = "aarch64")]
#[inline(always)]
/// Read the aarch64 CPU's timestamp counter from the cntvct_el0 register.
pub fn read_cpu_timestamp_counter() -> u64 {
    use core::arch::asm;

    let mut t: u64;

    unsafe {
        asm!(
            "mrs {t}, cntvct_el0",
            t = out(reg) t,
        );
    }

    t
}

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
#[inline(always)]
/// Return zero instead of the CPU's timestamp counter, because this is not a CPU we support.
pub fn read_cpu_timestamp_counter() -> u64 {
    // TODO(yan): Support more platforms? WASM maybe, if it even has precise timings.
    0
}

// SAFETY: This function exists to avoid regular indexing, which would create a reference.
#[doc(hidden)]
#[inline(always)]
pub unsafe fn slice_get<T>(slice_ptr: *const [T], index: usize) -> *const T {
    unsafe { (slice_ptr as *const T).add(index) }
}

// SAFETY: This function exists to avoid regular indexing, which would create a reference.
#[doc(hidden)]
#[inline(always)]
pub unsafe fn slice_get_mut<T>(slice_ptr: *mut [T], index: usize) -> *mut T {
    unsafe { (slice_ptr as *mut T).add(index) }
}

// A compile time hash of the zone label, it produces indices in range `1..PROFILER_ZONE_COUNT`.
//
// Never produces zero, because zone 0 is reserved for the root zone, so that we can remove branches
// from the profiling code.
//
// The hash algorithm is FNV 1a, slightly tweaked so that it avoids things that don't work at
// compile time, or would require nightly features to do so. We never invoke the hash at runtime.
#[doc(hidden)]
pub const fn hash_zone_label(zone_label: &'static str) -> usize {
    let bytes = zone_label.as_bytes();

    const SEED: u64 = 0xcbf29ce484222325;
    let mut hash = SEED;

    let mut i: usize = 0;
    while i < bytes.len() {
        let byte = bytes[i];

        hash ^= byte as u64;
        hash = hash.wrapping_mul(0x100000001b3);

        i += 1;
    }

    // Wrap around zone count, but make sure to never output zero.
    (hash as usize % (PROFILER_ZONE_COUNT - 1)) + 1
}

#[doc(hidden)]
pub struct Block {
    pub tsc_start: u64,
    pub tsc_elapsed_incl: u64,
    pub zone_label: &'static str,
    pub zone_idx: usize,
    pub parent_idx: usize,
    pub prof: *mut Profiler,
}

#[doc(hidden)]
impl Drop for Block {
    #[inline(always)]
    fn drop(&mut self) {
        let tsc_end = read_cpu_timestamp_counter();

        // SAFETY: The code below is simple in principle, but has to be written with raw pointers,
        // so that the LLVM Grinch doesn't eat our Christmas code. The &raw mut (formerly the
        // ptr::addr_of_mut macro) shenanigans are here to prevent creating a &mut to the static
        // memory of the profiler. In Rust, having multiple mutable references pointing to the same
        // memory is a big no-no, even if you never use those references. In safe Rust, the
        // borrowchecker prevents us from doing it, but we are accessing a mutable static, which is
        // unsafe in and of itself precisely for the reason that Rust gives those references 'static
        // lifetimes, and thus doesn't check for lifetime overlap. Also maybe for some thread
        // synchronization reasons.
        unsafe {
            //
            // Get *mut pointers to everything we'll need to access.
            //
            let zones = &raw mut (*(self.prof)).zones;

            let zone = slice_get_mut(zones, self.zone_idx);
            let zone_idx = &raw mut (*(self.prof)).zone_idx;
            let zones_conflicting = &raw mut (*(self.prof)).zones_conflicting;

            let parent = slice_get_mut(zones, self.parent_idx);

            //
            // See if our hashed zone id conflicts and update the 'zones_conflicting' field.
            //
            // TODO(yan): @Speed The overhead of doing this is measurable, but unless we find a way
            // to generate unique indices at compile time, we can't not do this. See proc macro TODO
            // comment below.
            *zones_conflicting |= ((*zone).label != "") & (self.zone_label != (*zone).label);

            //
            // Update zone hit count.
            //
            (*zone).hit_count = (*zone).hit_count.wrapping_add(1);

            //
            // Update timings in current and parent profile zone.
            //
            // Current zone's exclusive timings are added to. Parent zone's exclusive timings are
            // subtracted from. Current zone's inclusive timings are overwritten with the value we
            // compute here, so that we handle recursively nested zones.
            //
            let tsc_elapsed = tsc_end.wrapping_sub(self.tsc_start);

            // TODO(yan): @Speed Even Casey didn't find a way of not using the label, but this too
            // is an extra write we theoretically don't need, if we had metaprogramming that could
            // do all the zone assigning at compile time. In Rust, this would have to be a proc
            // macro that sees the entire crate.
            //
            // UPDATE: So it turns out it *should* be possible to run a proc macro on the entire
            // crate. If this is the case, we can do the following performance and usability
            // improvements.
            //
            // - We won't have to define the profiler ourselves, there will be a macro for it:
            //   #![profile_crate]
            //
            // - We won't have to check for zone conflicts or even hash zone labels. Instead we'll
            //   just assign unique IDs from the proc macro metaprogram.
            //
            // - We won't have to have a PROFILER_ZONE_COUNT. Instead, the metaprogram will just count how
            //   many zones we need and reserve that space for us.
            //
            // - We won't have to write the zone labels at runtime. Instead they'll be assigned at
            //   compile time, and will be able to live in a side array, so that we won't have to
            //   touch them during profiling at all, only use them get names.
            //
            // - We won't have to pass zone labels into the profile_zone! macro. Instead we can use
            //   Rust paths to identify them,
            //   e.g. "module1::Struct1::associated_function1::zone_label1". We can, however still
            //   add zone labels to add more descriptive names.
            //
            // Turns out that crate-level inner-attribute proc macros require at least two nightly
            // features (custom_inner_attributes and proc_macro_hygiene), and even with these they
            // fail, because they try access compiler-generated preludes for crates and
            // whatnot. While it would be cool to cut down on the overhead, at the moment it doesn't
            // seem doable without external metaprogramming.
            //
            // https://github.com/rust-lang/rust/issues/54726
            // https://github.com/rust-lang/rust/issues/54727
            // https://github.com/rust-lang/rust/issues/41430
            (*zone).label = self.zone_label;
            (*zone).tsc_elapsed_incl = self.tsc_elapsed_incl.wrapping_add(tsc_elapsed);
            (*zone).tsc_elapsed_excl = (*zone).tsc_elapsed_excl.wrapping_add(tsc_elapsed);

            (*parent).tsc_elapsed_excl = (*parent).tsc_elapsed_excl.wrapping_sub(tsc_elapsed);

            //
            // Set the parent zone as current.
            //
            *zone_idx = self.parent_idx;
        }
    }
}
