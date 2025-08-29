#![allow(clippy::comparison_to_empty)]

use std::time::Instant;

use wfprof::Profiler;

// The profiling zones are stored in global variable called PROFILER. Define it once in the program
// and import it wherever there's something you need to profile. We can also use the one defined in
// the profiler crate itself, either for convenience, or to coordinate profiling of multiple
// libraries that do not know of each other.
//
// Do not access the profiler from multiple threads, it is not synchronized in any way (the
// profile_zone! macros hide the unsafe from you for convenience). Synchronization and inter-core
// communication is orders of magnitude more expensive than what this profiler does. To use the
// profiler, either make sure your program only accesses it from a single thread, or extract the
// code you want to measure to an smaller program and profile there.
static mut PROFILER: Profiler = Profiler::new();

// Because we didn't use the PROFILER instance provded by the library, we have to point the
// profile_zone macro invocations to the right place. We could either do that manually at each
// invocation site, but we can also define our own macro for convenience.
macro_rules! profile_zone {
    ($zone_label:expr) => {
        wfprof::profile_zone!($zone_label, PROFILER)
    };
}

fn main() {
    let mut rng = oorandom::Rand32::new(42);

    // Prepare data we'll be using later.

    const NUMBER_COUNT: usize = 1000000;
    let mut numbers = Vec::with_capacity(NUMBER_COUNT);
    for _ in 0..NUMBER_COUNT {
        numbers.push(rng.rand_i32());
    }

    // To determine the scale of the CPU timestamps, which are dependent on the CPU frequency, we
    // also measure regular OS time both before and after the run of our program.
    let time_start = Instant::now();
    unsafe {
        Profiler::start(&raw mut PROFILER);
    }

    // First call the functions we'll be profiling and print their results so the compiler doesn't
    // delete them.
    let result1 = sum_numbers_twice(&numbers);
    let result2 = a(3);

    unsafe {
        Profiler::end(&raw mut PROFILER, "MAIN");
    }
    let time_end = Instant::now();

    println!("result 1: {result1}");
    println!("result 2: {result2}");
    println!();

    // Read zone data from profiler and print them out, sorted by exclusive time, highest to lowest.
    //
    // SAFETY: While it would be safe to make a single mutable reference once the profiling is over,
    // instead copy the data out, just to be safe. We don't want our male cat getting pregnant.
    let mut profiler_zones = unsafe { PROFILER.zones };
    let profiler_zones_conflicting = unsafe { PROFILER.zones_conflicting };

    let tsc_elapsed = profiler_zones[0].tsc_elapsed_incl;
    let time_elapsed = time_end - time_start;
    let tsc_to_time = time_elapsed.as_secs_f64() / tsc_elapsed as f64;

    profiler_zones[1..].sort_unstable_by(|a, b| {
        let time_a = a.tsc_elapsed_excl;
        let time_b = b.tsc_elapsed_excl;

        time_a.cmp(&time_b).reverse()
    });

    println!("Profiling results:");
    println!();

    if profiler_zones_conflicting {
        println!("Detected conflict! Some zones were written to with different labels. Results are incorrect.");
        println!("Increase the size of the zone storage to fix.");
        println!();
    }

    #[rustfmt::skip]
    println!(" ZONE NAME            | HIT COUNT       | EXCLUSIVE TIME              | INCLUSIVE TIME");
    println!("----------------------------------------------------------------------------------------------------");

    let zone0 = &profiler_zones[0];
    println!(
        " {:<20} | {:>15} | {:>15.9}ms ({:>6.2}%) | {:>15.9}ms ({:>6.2}%)",
        zone0.label,
        zone0.hit_count,
        zone0.tsc_elapsed_excl as f64 * tsc_to_time * 1000.0,
        100.0 * zone0.tsc_elapsed_excl as f64 * tsc_to_time / time_elapsed.as_secs_f64(),
        zone0.tsc_elapsed_incl as f64 * tsc_to_time * 1000.0,
        100.0 * zone0.tsc_elapsed_incl as f64 * tsc_to_time / time_elapsed.as_secs_f64(),
    );

    println!("----------------------------------------------------------------------------------------------------");
    for zone in &profiler_zones[1..] {
        if zone.label != "" {
            println!(
                " {:<20} | {:>15} | {:>15.9}ms ({:>6.2}%) | {:>15.9}ms ({:>6.2}%)",
                zone.label,
                zone.hit_count,
                zone.tsc_elapsed_excl as f64 * tsc_to_time * 1000.0,
                100.0 * zone.tsc_elapsed_excl as f64 * tsc_to_time / time_elapsed.as_secs_f64(),
                zone.tsc_elapsed_incl as f64 * tsc_to_time * 1000.0,
                100.0 * zone.tsc_elapsed_incl as f64 * tsc_to_time / time_elapsed.as_secs_f64(),
            );
        }
    }
}

// Note: the remaining functions in this file have #[inline(never)], so that their assembly can be
// viewed independent of main, which has a lot of cruft.

#[inline(never)]
fn sum_numbers_twice(numbers: &[i32]) -> i64 {
    // To time a block, use the profile_zone! macro. It measures time and inserts a small struct
    // into the local scope. The struct has a destructor which sums the time elapsed in the block
    // back to the PROFILER global in static memory once it goes out of scope.

    // "zone 1" profile_zone! essentially measures the whole duration of this function.
    profile_zone!("zone 1");

    let mut a: i64 = 0;
    for &n in numbers {
        // This measures the duration of this loop body. The loop body is entered many times, and
        // their times are summed up.
        profile_zone!("zone 2");
        a += i64::from(n);
    }

    let mut b: i64 = 0;
    {
        // This zone measures the duration of the enclosing block around the loop, which has less
        // overhead than measuring inside the loop body, because we only measure and write the
        // measurements time once.
        profile_zone!("zone 3");
        for &n in numbers {
            b += i64::from(n);
        }
    }

    // A few notes on profiling overhead:
    //
    // Time measuring is done with the RDTSC instruction on x64 and the cntvct_el0 register
    // aarch64. On modern (post 2010?) machines, RDTSC counts the "invariant TSC", meaning it counts
    // elapsed cycles as though core frequencies never changed (e.g. because of boost frequencies)
    // [1]. On aarch64, the cntvct_el0 is supposed to be a "an off-core clock, typically running at
    // 24MHz" [2].
    //
    // [1] https://software.intel.com/en-us/download/intel-64-and-ia-32-architectures-sdm-combined-volumes-1-2a-2b-2c-2d-3a-3b-3c-3d-and-4
    //     (ctrl-f for "rdtsc" and "invariant tsc")
    //
    // [2] https://cpucycles.cr.yp.to/counters.html
    //
    // Opening a profiling block (calling the profile_zone! macro) does a small amount of reads and
    // writes from and to (hopefully hot) locations in static memory, and then measures the block's
    // start time by reading the timestamp counter.
    //
    // Closing the profiling block (when the variable created in the block goes out of scope),
    // measures the block's end time, does a few more reads and writes to static memory,
    // interspersed with a few arithmetic operations.
    //
    // If the code you are measuring doesn't sound significantly larger than the description above,
    // consider sacrificing usability and hand-rolling your own profiling instead, as it can be
    // boiled down to just two measurements, one integer addition and one subtraction, and some
    // register allocation pressure.

    a + b
}

#[inline(never)]
fn a(n: u64) -> u64 {
    // Here we show that the profiler handles recursion. Function A can call function B, which can
    // call function A again. In these conditions, we still need to report correct inclusive and
    // exclusive times for each profiled zone, i.e. not count time twice for A even if the call
    // hierachy looks like this:
    //
    //             A
    // |------------------------|
    //     B     B        B
    //  |----| |---|  |-------|
    //    A               A
    //   |-|            |---|
    //

    profile_zone!("A");

    if n == 0 {
        return 1;
    }

    let mut result = 0;
    for i in 0..100 {
        result += b(i, n - 1);
    }

    result
}

#[inline(never)]
fn b(i: u64, n: u64) -> u64 {
    profile_zone!("B");
    i + n + a(n)
}
