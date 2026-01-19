extern crate proc_macro;

use std::collections::HashSet;

use proc_macro::TokenStream;
use proc_macro::TokenTree;
use proc_macro2::Ident;
use proc_macro2::Literal;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::spanned::Spanned as _;

// TODO(jt): #genflags @Cleanup Remove 'keep'. Instead add derive(flags), which takes the enum's numeric value
// as left shift operand, generating a new type and automatically preserving the enum:
//
// #[derive(Flags)]
// #[repr(u32)]
// enum EntityType {
//     Hero = 0,
//     Monster = 1,
// }
//
// Derive generates this:
//
// struct EntityTypeFlags(u32);
// impl EntityTypeFlags {
//     pub const Hero    = 1 << (EntityType::Hero as u32);
//     pub const Monster = 1 << (EntityType::Monster as u32)
// }
//
// If there's problems with making this a derive, just make it a full blown macro attribute: #[genflags("EntityTypeFlags")].
//
// TODO(jt): @Correctness Add assertions against unknown bits in every operation. If we ever
// bytecast, or load bitflags from untrusted sources, we can at least learn about it in debug
// builds.
//
// TODO(jt): Preserve attributes from enum variants?  (Might be obsolete if we do #genflags)
// TODO(jt): Preserve pub(whatever) from source item. (Might be obsolete if we do #genflags)
//
// TODO(jt): Add support for compound variants to #[flags]. Compound variants can have the form: X | Y | Z,
// where they always have to refer to an already defined literal variant.
enum FlagsIntType {
    U8,
    U16,
    U32,
    U64,
}

impl quote::ToTokens for FlagsIntType {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        use quote::TokenStreamExt;
        match self {
            Self::U8 => tokens.append(syn::Ident::new("u8", Span::call_site())),
            Self::U16 => tokens.append(syn::Ident::new("u16", Span::call_site())),
            Self::U32 => tokens.append(syn::Ident::new("u32", Span::call_site())),
            Self::U64 => tokens.append(syn::Ident::new("u64", Span::call_site())),
        }
    }
}

#[proc_macro_attribute]
pub fn flags(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut ty: Option<FlagsIntType> = None;
    let mut keep: Option<String> = None;

    let mut parsing_keep = 0;

    for attr_token in attr {
        match attr_token {
            TokenTree::Ident(ident) => {
                if parsing_keep != 0 {
                    panic!("Unexpected identifier, {ident}");
                }

                let ident_str = ident.to_string();
                match ident_str.as_str() {
                    "u8" => ty = Some(FlagsIntType::U8),
                    "u16" => ty = Some(FlagsIntType::U16),
                    "u32" => ty = Some(FlagsIntType::U32),
                    "u64" => ty = Some(FlagsIntType::U64),
                    "keep" => {
                        parsing_keep = 1;
                    }

                    _ => panic!("Unsupported option in #[flags] attributes"),
                }
            }

            TokenTree::Punct(punct) => {
                let c = punct.as_char();

                if parsing_keep == 0 && c != ',' {
                    panic!("Unexpected punctuation, expected ','");
                }

                if parsing_keep == 1 && c != '=' {
                    panic!("Unexpected punctuation, expected '='");
                }

                if parsing_keep == 2 {
                    panic!("Unexpected punctuation, expected keep identifier");
                }

                if parsing_keep == 1 {
                    parsing_keep = 2;
                }
            }

            TokenTree::Literal(literal) => {
                if parsing_keep != 2 {
                    panic!("Unexpected literal");
                }

                let lit = literal.to_string();

                if lit.starts_with('"') && lit.ends_with('"') {
                    keep = Some(String::from(&lit[1..lit.len() - 1]));
                } else {
                    panic!("Keep identifier must be a string literal");
                }

                parsing_keep = 0;
            }

            _ => panic!("Unsupported token in #[flags] attributes"),
        }
    }

    if let Some(ty) = ty {
        // Technically, this is not a derive, but the input is very derive-like, so
        // this pre-made parser works for us.
        let parsed_item = syn::parse_macro_input!(item as syn::DeriveInput);

        let result = match &parsed_item.data {
            syn::Data::Struct(_) => Err(syn::Error::new(
                Span::call_site(),
                "Struct are not supported for making flags",
            )),
            syn::Data::Enum(data) => flags_enum(&parsed_item, data, ty, keep),
            syn::Data::Union(_) => Err(syn::Error::new(
                Span::call_site(),
                "Unions are not supported for making flags",
            )),
        };

        result.unwrap_or_else(|err| err.to_compile_error()).into()
    } else {
        let err = quote! {
            compile_error!("Must provide an int type in #[flags] attribute");
        };

        err.into()
    }
}

fn flags_enum(
    input: &syn::DeriveInput,
    data: &syn::DataEnum,
    ty: FlagsIntType,
    keep: Option<String>,
) -> syn::Result<TokenStream2> {
    // TODO(jt): @Correctness Validate that the enum has no generic parameters.

    let ident = &input.ident;
    let attrs = &input.attrs;

    let mut default_variant_index: Option<usize> = None;
    let mut values: HashSet<u128> = HashSet::new();
    let mut lit_variants: Vec<(syn::Ident, syn::Expr, syn::LitInt)> = Vec::new();

    for (variant_index, variant) in data.variants.iter().enumerate() {
        for variant_attr in &variant.attrs {
            if let syn::Meta::Path(path) = &variant_attr.meta {
                if path.is_ident("default") {
                    if default_variant_index.is_some() {
                        return Err(syn::Error::new(
                            variant_attr.span(),
                            "#[default] attribute defined multiple times",
                        ));
                    }

                    default_variant_index = Some(variant_index);
                }
            }
        }

        match &variant.fields {
            syn::Fields::Named(_) => {
                return Err(syn::Error::new(
                    variant.ident.span(),
                    "Named field variants are not supported",
                ));
            }

            syn::Fields::Unnamed(_) => {
                return Err(syn::Error::new(
                    variant.ident.span(),
                    "Unnamed field variants are not supported",
                ));
            }

            syn::Fields::Unit => match &variant.discriminant {
                Some((_, expr)) => match expr {
                    syn::Expr::Binary(binary_expr) => match binary_expr.op {
                        syn::BinOp::Shl(_) => {
                            let left = match binary_expr.left.as_ref() {
                                syn::Expr::Lit(lit_expr) => match &lit_expr.lit {
                                    syn::Lit::Int(int_lit) => match int_lit.base10_parse::<u128>() {
                                        Ok(value) => value,
                                        Err(_) => return Err(syn::Error::new(int_lit.span(), "Can't parse literal")),
                                    },

                                    _ => {
                                        return Err(syn::Error::new(
                                            binary_expr.left.span(),
                                            "Shift operands must be integers",
                                        ));
                                    }
                                },

                                _ => return Err(syn::Error::new(binary_expr.left.span(), "Unsupported literal")),
                            };

                            let right = match binary_expr.right.as_ref() {
                                syn::Expr::Lit(lit_expr) => match &lit_expr.lit {
                                    syn::Lit::Int(int_lit) => match int_lit.base10_parse::<u32>() {
                                        Ok(value) => value,
                                        Err(_) => return Err(syn::Error::new(int_lit.span(), "Can't parse literal")),
                                    },

                                    _ => {
                                        return Err(syn::Error::new(
                                            binary_expr.right.span(),
                                            "Shift operands must be integers",
                                        ));
                                    }
                                },

                                _ => return Err(syn::Error::new(binary_expr.right.span(), "Unsupported literal")),
                            };

                            let shifted = left << right;
                            match ty {
                                FlagsIntType::U8 => {
                                    if let Err(_) = u8::try_from(shifted) {
                                        return Err(syn::Error::new(expr.span(), "Literal too large to fit in u8"));
                                    }
                                }
                                FlagsIntType::U16 => {
                                    if let Err(_) = u16::try_from(shifted) {
                                        return Err(syn::Error::new(expr.span(), "Literal too large to fit in u16"));
                                    }
                                }
                                FlagsIntType::U32 => {
                                    if let Err(_) = u32::try_from(shifted) {
                                        return Err(syn::Error::new(expr.span(), "Literal too large to fit in u32"));
                                    }
                                }
                                FlagsIntType::U64 => {
                                    if let Err(_) = u64::try_from(shifted) {
                                        return Err(syn::Error::new(expr.span(), "Literal too large to fit in u64"));
                                    }
                                }
                            }

                            if shifted.count_ones() != 1 {
                                return Err(syn::Error::new(expr.span(), "Literal doesn't have exactly one set bit"));
                            }

                            if values.contains(&shifted) {
                                return Err(syn::Error::new(expr.span(), "Literal defined multiple times"));
                            }

                            // Bytemuck's derive for CheckedBitPattern does not
                            // support binary expressions, and it seems
                            // unreasonable to make them do so.
                            //
                            // To be compatible with bytemuck, emit the shifted
                            // literal instead of the shl expression that
                            // produces it.
                            let mut literal = Literal::u128_unsuffixed(shifted);
                            literal.set_span(expr.span());

                            let int_lit = syn::LitInt::from(literal);

                            values.insert(shifted);
                            lit_variants.push((variant.ident.clone(), expr.clone(), int_lit));
                        }

                        _ => {
                            return Err(syn::Error::new(
                                expr.span(),
                                "Unsupported binary expression for literal",
                            ));
                        }
                    },

                    syn::Expr::Lit(lit_expr) => match &lit_expr.lit {
                        syn::Lit::Int(int_lit) => match int_lit.base10_parse::<u128>() {
                            Ok(value) => {
                                match ty {
                                    FlagsIntType::U8 => {
                                        if let Err(_) = u8::try_from(value) {
                                            return Err(syn::Error::new(expr.span(), "Literal too large to fit in u8"));
                                        }
                                    }
                                    FlagsIntType::U16 => {
                                        if let Err(_) = u16::try_from(value) {
                                            return Err(syn::Error::new(
                                                expr.span(),
                                                "Literal too large to fit in u16",
                                            ));
                                        }
                                    }
                                    FlagsIntType::U32 => {
                                        if let Err(_) = u32::try_from(value) {
                                            return Err(syn::Error::new(
                                                expr.span(),
                                                "Literal too large to fit in u32",
                                            ));
                                        }
                                    }
                                    FlagsIntType::U64 => {
                                        if let Err(_) = u64::try_from(value) {
                                            return Err(syn::Error::new(
                                                expr.span(),
                                                "Literal too large to fit in u64",
                                            ));
                                        }
                                    }
                                }

                                if value.count_ones() != 1 {
                                    return Err(syn::Error::new(
                                        expr.span(),
                                        "Literal doesn't have exactly one set bit",
                                    ));
                                }

                                if values.contains(&value) {
                                    return Err(syn::Error::new(expr.span(), "Literal defined multiple times"));
                                }

                                values.insert(value);
                                lit_variants.push((variant.ident.clone(), expr.clone(), int_lit.clone()));
                            }

                            Err(_) => return Err(syn::Error::new(expr.span(), "Can't parse literal")),
                        },

                        _ => return Err(syn::Error::new(expr.span(), "Unsupported literal")),
                    },

                    _ => return Err(syn::Error::new(expr.span(), "Unsupported discriminant expression")),
                },

                None => {
                    return Err(syn::Error::new(
                        variant.ident.span(),
                        "Unit field variants without explicit discriminant are not (yet) supported",
                    ));
                }
            },
        }
    }

    let mut keep_variant_sources: Vec<TokenStream2> = Vec::new();
    let mut keep_to_flags_sources: Vec<TokenStream2> = Vec::new();
    let mut variant_sources: Vec<TokenStream2> = Vec::new();
    let mut all_variant_sources: Vec<TokenStream2> = Vec::new();
    let mut value_sources: Vec<TokenStream2> = Vec::new();
    let mut display_sources: Vec<TokenStream2> = Vec::new();

    for (variant_ident, variant_expr, variant_lit) in &lit_variants {
        let keep_variant_source = quote! {
            #variant_ident = #variant_lit,
        };

        let keep_to_flags_source = quote! {
            Self::#variant_ident => #ident::#variant_ident,
        };

        let lit_variant_source = quote! {
            pub const #variant_ident: Self = Self(#variant_expr);
        };

        let all_variant_source = if all_variant_sources.len() > 0 {
            quote! { | Self::#variant_ident.0 }
        } else {
            quote! { Self::#variant_ident.0 }
        };

        let value_source = quote! {
            Self::#variant_ident,
        };

        let display_source = quote! {
            Self::#variant_ident => f.pad(stringify!(#variant_ident)),
        };

        keep_variant_sources.push(keep_variant_source);
        keep_to_flags_sources.push(keep_to_flags_source);
        variant_sources.push(lit_variant_source);
        all_variant_sources.push(all_variant_source);
        value_sources.push(value_source);
        display_sources.push(display_source);
    }

    let mut keep_sources: Vec<TokenStream2> = Vec::new();
    if let Some(keep) = keep {
        let keep_ident = Ident::new(&keep, Span::call_site());

        let mut keep_default_sources: Vec<TokenStream2> = Vec::new();
        if let Some(variant_index) = default_variant_index {
            let variant_ident = lit_variants[variant_index].0.clone();
            let keep_default_source = quote! {
                impl core::default::Default for #keep_ident {
                    fn default() -> Self {
                        Self::#variant_ident
                    }
                }
            };

            keep_default_sources.push(keep_default_source);
        }

        let keep_source = quote! {
            #[repr(#ty)]
            #[allow(non_camel_case_types)]
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
            #(#attrs)*
            pub enum #keep_ident {
                #(#keep_variant_sources)*
            }

            impl #keep_ident {
                pub const fn values() -> &'static [Self] {
                    &[
                        #(#value_sources)*
                    ]
                }

                pub const fn to_flags(&self) -> #ident {
                    match self {
                        #(#keep_to_flags_sources)*
                    }
                }
            }

            #(#keep_default_sources)*

            impl core::fmt::Display for #keep_ident {
                fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                    match self {
                        #(#display_sources)*
                    }
                }
            }
        };

        keep_sources.push(keep_source);
    }

    // TODO(jt): Generating Default for flags based on the enum's default is surprising, but so is
    // ignoring the enum's Default and generating empty flags. Should we force something more
    // explicit? Perhaps prohibiting enum #[default] altogether, so that if we want default, we have
    // to do a manual impl (but ideally we don't want it?)
    //
    // UPDATE: #genflags Disallow Default for #[flags]. Allow Default for #[genflags], but don't
    // carry it over.
    let mut default_sources: Vec<TokenStream2> = Vec::new();
    if let Some(variant_index) = default_variant_index {
        let variant_ident = lit_variants[variant_index].0.clone();
        let default_source = quote! {
            impl core::default::Default for #ident {
                fn default() -> Self {
                    Self::#variant_ident
                }
            }
        };

        default_sources.push(default_source);
    }

    let derive_source = if default_sources.len() == 0 {
        quote! { #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)] }
    } else {
        quote! { #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] }
    };

    Ok(quote! {
        #(#keep_sources)*

        #[repr(C)]
        #derive_source
        #(#attrs)*
        pub struct #ident(#ty);

        impl #ident {
            #(#variant_sources)*

            pub const NONE: Self = Self(0);
            pub const ALL: Self = Self(#(#all_variant_sources)*);

            #[inline]
            pub const fn from_bits(bits: #ty) -> Self {
                Self(Self::ALL.0 & bits)
            }

            #[inline]
            pub const fn values() -> &'static [Self] {
                &[
                    #(#value_sources)*
                ]
            }

            #[inline]
            pub const fn bits(&self) -> #ty {
                self.0
            }

            #[inline]
            pub const fn intersects(&self, other: Self) -> bool {
                self.0 & other.0 != 0
            }

            #[inline]
            pub const fn contains(&self, other: Self) -> bool {
                self.0 & other.0 == other.0
            }

            #[inline]
            pub const fn insert(&mut self, other: Self) -> bool {
                let orig = self.0;
                self.0 |= other.0;

                orig != self.0
            }

            #[inline]
            pub const fn remove(&mut self, other: Self) -> bool {
                let orig = self.0;
                self.0 &= !other.0;

                orig != self.0
            }

            #[inline]
            pub const fn toggle(&mut self, other: Self) {
                self.0 ^= other.0;
            }

            #[inline]
            pub const fn set(&mut self, other: Self, value: bool) {
                if value {
                    self.insert(other);
                } else {
                    self.remove(other);
                }
            }

            // TODO(yan): @Cleanup #ConstTraitImpls In April 2023, rustc removed const
            // trait impls. They want to add it back eventually. Until then, we have
            // regular const functions for the below.

            #[inline]
            pub const fn const_eq(self, other: Self) -> bool {
                self.0 == other.0
            }

            #[inline]
            pub const fn const_bitor(self, other: Self) -> Self {
                Self(self.0 | other.0)
            }

            #[inline]
            pub const fn const_bitand(self, other: Self) -> Self {
                Self(self.0 & other.0)
            }

            #[inline]
            pub const fn const_not(self) -> Self {
                Self(!self.0)
            }

            #[inline]
            pub const fn const_sub(self, other: Self) -> Self {
                Self(self.0 & !other.0)
            }
        }

        impl core::ops::BitOr for #ident {
            type Output = Self;

            #[inline]
            fn bitor(self, other: Self) -> Self {
                Self(self.0 | other.0)
            }
        }

        impl core::ops::BitOrAssign for #ident {
            #[inline]
            fn bitor_assign(&mut self, other: Self) {
                self.0 |= other.0;
            }
        }

        impl core::ops::BitAnd for #ident {
            type Output = Self;

            #[inline]
            fn bitand(self, other: Self) -> Self {
                Self(self.0 & other.0)
            }
        }

        impl core::ops::BitAndAssign for #ident {
            #[inline]
            fn bitand_assign(&mut self, other: Self) {
                self.0 &= other.0;
            }
        }

        impl core::ops::Not for #ident {
            type Output = Self;

            #[inline]
            fn not(self) -> Self {
                Self(!self.0)
            }
        }

        impl core::ops::Sub for #ident {
            type Output = Self;

            #[inline]
            fn sub(self, other: Self) -> Self {
                Self(self.0 & !other.0)
            }
        }

        impl core::ops::SubAssign for #ident {
            #[inline]
            fn sub_assign(&mut self, other: Self) {
                self.0 &= !other.0;
            }
        }

        #(#default_sources)*

        // TODO(jt): Make Disply impl for flag type better by decomposing the value and generating
        // the text.
        impl core::fmt::Display for #ident {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                match *self {
                    #(#display_sources)*
                    Self(other) => write!(f, "{}({})", stringify!(#ident), other),
                }
            }
        }
    })
}
