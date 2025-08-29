extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

// TODO(jt): Support rename attribute for enum variants.
// TODO(jt): Support version attribute for enum variants.
// TODO(jt): Support all regular struct field attributes on named struct fields in enum variants.

#[proc_macro_derive(Serialize, attributes(serialize))]
pub fn derive_serialize(input: TokenStream) -> TokenStream {
    let parsed_input = syn::parse_macro_input!(input as syn::DeriveInput);
    serialize(parsed_input)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[proc_macro_derive(Deserialize, attributes(serialize))]
pub fn derive_deserialize(input: TokenStream) -> TokenStream {
    let parsed_input = syn::parse_macro_input!(input as syn::DeriveInput);
    deserialize(parsed_input)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

fn serialize(input: syn::DeriveInput) -> syn::Result<TokenStream2> {
    match &input.data {
        syn::Data::Struct(data) => serialize_struct(&input, data),
        syn::Data::Enum(data) => serialize_enum(&input, data),
        syn::Data::Union(_) => Err(syn::Error::new(Span::call_site(), "Unions are not supported, Dave")),
    }
}

fn serialize_struct(input: &syn::DeriveInput, data: &syn::DataStruct) -> syn::Result<TokenStream2> {
    let ident = &input.ident;
    let item_attrs = parse_item_attrs(&input.attrs)?;

    if !matches!(data.fields, syn::Fields::Named(_)) && item_attrs.version != None {
        return Err(syn::Error::new_spanned(
            input,
            "'version' can only be defined on regular structs",
        ));
    }

    if !matches!(data.fields, syn::Fields::Named(_)) && item_attrs.migrate {
        return Err(syn::Error::new_spanned(
            input,
            "'migrate' can only be defined on regular structs",
        ));
    }

    if item_attrs.migrate && item_attrs.version == None {
        return Err(syn::Error::new_spanned(
            input,
            "'migrate' requires 'version' to be defined on the struct",
        ));
    }

    let rename_ident = match item_attrs.rename {
        Some(rename_ident) => rename_ident,
        None => ident.clone(),
    };

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut field_codes: Vec<TokenStream2> = Vec::new();
    match &data.fields {
        syn::Fields::Named(fields) => {
            for field in &fields.named {
                let field_ident = field.ident.as_ref().unwrap();
                let field_attrs = parse_field_attrs(&field.attrs)?;

                let rename_field_ident = match field_attrs.rename {
                    Some(rename_field_ident) => rename_field_ident,
                    None => field_ident.clone(),
                };

                if !field_attrs.skip {
                    let field_code = quote! {
                        f.field(stringify!(#rename_field_ident), |f| self.#field_ident.serialize(f));
                    };

                    field_codes.push(field_code);
                }
            }
        }
        syn::Fields::Unnamed(fields) => {
            for (i, field) in fields.unnamed.iter().enumerate() {
                let field_index = syn::Index::from(i);
                let field_attrs = parse_field_attrs(&field.attrs)?;

                if field_attrs.skip {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'skip' is not valid on unnamed struct fields",
                    ));
                }
                if field_attrs.optional {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'optional' is not valid on unnamed struct fields",
                    ));
                }
                if field_attrs.version_from.is_some() {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'version_from' is not valid on unnamed struct fields",
                    ));
                }
                if field_attrs.version_to.is_some() {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'version_to' is not valid on unnamed struct fields",
                    ));
                }
                if field_attrs.rename.is_some() {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'rename' is not valid on unnamed struct fields",
                    ));
                }

                let field_code = quote! {
                    f.item(|f| self.#field_index.serialize(f));
                };

                field_codes.push(field_code);
            }
        }
        syn::Fields::Unit => (),
    }

    let fields_len = data.fields.len();
    let serialize_code = match data.fields {
        syn::Fields::Named(_) => {
            if let Some(version) = item_attrs.version {
                quote! {
                    f.dictionary_struct_with_version(
                        stringify!(#rename_ident),
                        #version,
                        #fields_len > wfserialize::DICTIONARY_LIKE_MULTILINE_THRESHOLD,
                        |f| {
                            #(#field_codes)*
                        },
                    );
                }
            } else {
                quote! {
                    f.dictionary_struct(
                        stringify!(#rename_ident),
                        #fields_len > wfserialize::DICTIONARY_LIKE_MULTILINE_THRESHOLD,
                        |f| {
                            #(#field_codes)*
                        },
                    );
                }
            }
        }
        syn::Fields::Unnamed(_) => {
            quote! {
                f.tuple_struct(
                    stringify!(#rename_ident),
                    #fields_len > wfserialize::ARRAY_LIKE_MULTILINE_THRESHOLD,
                    |f| {
                        #(#field_codes)*
                    },
                );
            }
        }
        syn::Fields::Unit => {
            quote! {
                f.unit_struct(stringify!(#rename_ident));
            }
        }
    };

    Ok(quote! {
        const _: () = {
            impl #impl_generics wfserialize::Serialize for #ident #ty_generics #where_clause {
                fn serialize<A: core::alloc::Allocator>(&self, f: &mut wfserialize::Formatter<A>) {
                    #serialize_code
                }
            }
        };
    })
}

fn serialize_enum(input: &syn::DeriveInput, data: &syn::DataEnum) -> syn::Result<TokenStream2> {
    let ident = &input.ident;
    let item_attrs = parse_item_attrs(&input.attrs)?;

    // Enums don't have their type name in the serialized text, so no rename here. They also don't
    // have versions or migration.
    if item_attrs.version.is_some() {
        return Err(syn::Error::new_spanned(input, "'version' is not valid on enums"));
    }
    if item_attrs.migrate {
        return Err(syn::Error::new_spanned(input, "'migrate' is not valid on enums"));
    }
    if item_attrs.rename.is_some() {
        return Err(syn::Error::new_spanned(input, "'rename' is not valid on enums"));
    }

    // ... also no allocator here, as it is only relevant for deserialization.

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut match_arm_codes: Vec<TokenStream2> = Vec::new();
    for variant in &data.variants {
        let variant_ident = &variant.ident;

        let fields_len = variant.fields.len();
        let match_arm_code = match &variant.fields {
            syn::Fields::Named(fields) => {
                let mut destructuring_codes: Vec<TokenStream2> = Vec::new();
                let mut field_codes: Vec<TokenStream2> = Vec::new();

                for field in &fields.named {
                    let field_ident = field.ident.as_ref().unwrap();
                    let field_attrs = parse_field_attrs(&field.attrs)?;

                    if field_attrs.skip {
                        return Err(syn::Error::new_spanned(field, "'skip' is not valid on enum fields"));
                    }
                    if field_attrs.optional {
                        return Err(syn::Error::new_spanned(field, "'optional' is not valid on enum fields"));
                    }
                    if field_attrs.version_from.is_some() {
                        return Err(syn::Error::new_spanned(
                            field,
                            "'version_from' is not valid on enum fields",
                        ));
                    }
                    if field_attrs.version_to.is_some() {
                        return Err(syn::Error::new_spanned(
                            field,
                            "'version_to' is not valid on enum fields",
                        ));
                    }
                    if field_attrs.rename.is_some() {
                        return Err(syn::Error::new_spanned(field, "'rename' is not valid on enum fields"));
                    }

                    let destructuring_code = quote! {
                        #field_ident,
                    };
                    let field_code = quote! {
                        f.field(stringify!(#field_ident), |f| #field_ident.serialize(f));
                    };

                    destructuring_codes.push(destructuring_code);
                    field_codes.push(field_code);
                }

                quote! {
                    #ident::#variant_ident { #(#destructuring_codes)* } => {
                        f.dictionary_struct(
                            stringify!(#variant_ident),
                            #fields_len > wfserialize::DICTIONARY_LIKE_MULTILINE_THRESHOLD,
                            |f| {
                                #(#field_codes)*
                            },
                        );
                    }
                }
            }
            syn::Fields::Unnamed(fields) => {
                let mut destructuring_codes: Vec<TokenStream2> = Vec::new();
                let mut field_codes: Vec<TokenStream2> = Vec::new();

                for (i, field) in fields.unnamed.iter().enumerate() {
                    let field_ident = syn::Ident::new(&format!("__SERIALIZE__{i}"), Span::call_site());
                    let field_attrs = parse_field_attrs(&field.attrs)?;

                    if field_attrs.skip {
                        return Err(syn::Error::new_spanned(field, "'skip' is not valid on enum fields"));
                    }
                    if field_attrs.optional {
                        return Err(syn::Error::new_spanned(field, "'optional' is not valid on enum fields"));
                    }
                    if field_attrs.version_from.is_some() {
                        return Err(syn::Error::new_spanned(
                            field,
                            "'version_from' is not valid on enum fields",
                        ));
                    }
                    if field_attrs.version_to.is_some() {
                        return Err(syn::Error::new_spanned(
                            field,
                            "'version_to' is not valid on enum fields",
                        ));
                    }
                    if field_attrs.rename.is_some() {
                        return Err(syn::Error::new_spanned(field, "'rename' is not valid on enum fields"));
                    }

                    let destructuring_code = quote! {
                        #field_ident,
                    };
                    let field_code = quote! {
                        f.item(|f| #field_ident.serialize(f));
                    };

                    destructuring_codes.push(destructuring_code);
                    field_codes.push(field_code);
                }

                quote! {
                    #ident::#variant_ident( #(#destructuring_codes)* ) => {
                        f.tuple_struct(
                            stringify!(#variant_ident),
                            #fields_len > wfserialize::ARRAY_LIKE_MULTILINE_THRESHOLD,
                            |f| {
                                #(#field_codes)*
                            },
                        );
                    }
                }
            }
            syn::Fields::Unit => {
                quote! {
                    #ident::#variant_ident => {
                        f.unit_struct(stringify!(#variant_ident))
                    }
                }
            }
        };

        match_arm_codes.push(match_arm_code);
    }

    Ok(quote! {
        const _: () = {
            impl #impl_generics wfserialize::Serialize for #ident #ty_generics #where_clause {
                fn serialize<A: core::alloc::Allocator>(&self, f: &mut wfserialize::Formatter<A>) {
                    match self {
                        #(#match_arm_codes)*
                    }
                }
            }
        };
    })
}

fn deserialize(input: syn::DeriveInput) -> syn::Result<TokenStream2> {
    match &input.data {
        syn::Data::Struct(data) => deserialize_struct(&input, data),
        syn::Data::Enum(data) => deserialize_enum(&input, data),
        syn::Data::Union(_) => Err(syn::Error::new(Span::call_site(), "Unions are not supported, Dave")),
    }
}

fn deserialize_struct(input: &syn::DeriveInput, data: &syn::DataStruct) -> syn::Result<TokenStream2> {
    let ident = &input.ident;
    let item_attrs = parse_item_attrs(&input.attrs)?;

    if !matches!(data.fields, syn::Fields::Named(_)) && item_attrs.version != None {
        return Err(syn::Error::new_spanned(
            input,
            "'version' can only be defined on regular structs",
        ));
    }

    if !matches!(data.fields, syn::Fields::Named(_)) && item_attrs.migrate {
        return Err(syn::Error::new_spanned(
            input,
            "'migrate' can only be defined on regular structs",
        ));
    }

    if item_attrs.migrate && item_attrs.version == None {
        // We require this, because we always need to know which version we are migrating to, even
        // if we don't know what version we are migrating from.
        return Err(syn::Error::new_spanned(
            input,
            "'migrate' requires 'version' to be defined on the struct",
        ));
    }

    // TODO(jt): @Cleanup The generic allocator feature is broken and likely never completely
    // worked? Maybe it doesn't matter, because for us, the allocator is almost always &'a Arena
    // (maybe except for tests). What do we do?
    //
    // TODO(yan): Add allocator_is_generic to know whether we should emit the
    // spliced impl_generics for it. Also for enums.
    let (allocator_ty, generic) = match item_attrs.allocator {
        Some(allocator_ty) => (allocator_ty, false),
        None => {
            let allocator_ty = syn::Type::Path(syn::TypePath {
                qself: None,
                path: syn::Path {
                    leading_colon: None,
                    segments: {
                        let mut segments_punct = syn::punctuated::Punctuated::new();
                        segments_punct.push(syn::PathSegment {
                            ident: syn::Ident::new("__SERIALIZE_A__", Span::call_site()),
                            arguments: syn::PathArguments::None,
                        });

                        segments_punct
                    },
                },
            });

            (allocator_ty, true)
        }
    };

    let rename_ident = match item_attrs.rename {
        Some(rename_ident) => rename_ident,
        None => ident.clone(),
    };

    // Unlike Serialize, the Deserialize trait has a generic parameter itself,
    // meaning we can't just cut-n-paste generics from the original type, but
    // instead we need to splice the generic parameter of the trait to the
    // impl_generics from the original type, but NOT the ty_generics or the
    // where_clause. We only need to do this, if the provided allocator
    // attribute is a generic type parameter and not a concrete type, and thus
    // needs to be present in the impl_generics.
    let mut impl_augmented_generics = input.generics.clone();

    // TODO(yan): Implement serialize(allocator_generic) for when the user provides
    // no concrete allocator, but wants to tie the allocator type parameter to
    // the allocator generic type defined on the struct.
    if generic {
        impl_augmented_generics
            .params
            .push(syn::GenericParam::Type(syn::TypeParam {
                attrs: Vec::new(),
                ident: syn::Ident::new("__SERIALIZE_A__", Span::call_site()),
                colon_token: Some(syn::token::Colon {
                    spans: [Span::call_site()],
                }),
                bounds: {
                    let mut bounds_punct = syn::punctuated::Punctuated::new();

                    bounds_punct.push(syn::TypeParamBound::Trait(syn::TraitBound {
                        paren_token: None,
                        modifier: syn::TraitBoundModifier::None,
                        lifetimes: None,
                        path: syn::Path {
                            leading_colon: None,
                            segments: {
                                let mut segments_punct = syn::punctuated::Punctuated::new();
                                segments_punct.push(syn::PathSegment {
                                    ident: syn::Ident::new("core", Span::call_site()),
                                    arguments: syn::PathArguments::None,
                                });
                                segments_punct.push(syn::PathSegment {
                                    ident: syn::Ident::new("alloc", Span::call_site()),
                                    arguments: syn::PathArguments::None,
                                });
                                segments_punct.push(syn::PathSegment {
                                    ident: syn::Ident::new("Allocator", Span::call_site()),
                                    arguments: syn::PathArguments::None,
                                });

                                segments_punct
                            },
                        },
                    }));

                    bounds_punct.push(syn::TypeParamBound::Trait(syn::TraitBound {
                        paren_token: None,
                        modifier: syn::TraitBoundModifier::None,
                        lifetimes: None,
                        path: syn::Path {
                            leading_colon: None,
                            segments: {
                                let mut segments_punct = syn::punctuated::Punctuated::new();
                                segments_punct.push(syn::PathSegment {
                                    ident: syn::Ident::new("core", Span::call_site()),
                                    arguments: syn::PathArguments::None,
                                });
                                segments_punct.push(syn::PathSegment {
                                    ident: syn::Ident::new("clone", Span::call_site()),
                                    arguments: syn::PathArguments::None,
                                });
                                segments_punct.push(syn::PathSegment {
                                    ident: syn::Ident::new("Clone", Span::call_site()),
                                    arguments: syn::PathArguments::None,
                                });

                                segments_punct
                            },
                        },
                    }));

                    bounds_punct
                },
                eq_token: None,
                default: None,
            }));
    }

    let (_, ty_generics, where_clause) = input.generics.split_for_impl();
    let (impl_generics, _, _) = impl_augmented_generics.split_for_impl();

    let deserialize_code = match &data.fields {
        syn::Fields::Named(fields) => {
            let mut field_decl_codes: Vec<TokenStream2> = Vec::new();
            let mut field_init_codes: Vec<TokenStream2> = Vec::new();
            for field in &fields.named {
                let field_ident = field.ident.as_ref().unwrap();
                let field_ty = &field.ty;
                let field_attrs = parse_field_attrs(&field.attrs)?;

                if field_attrs.skip && field_attrs.optional {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'skip' is not valid in combination with 'optional'",
                    ));
                }
                if field_attrs.skip && field_attrs.version_from.is_some() {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'skip' is not valid in combination with 'version_from'",
                    ));
                }
                if field_attrs.skip && field_attrs.version_to.is_some() {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'skip' is not valid in combination with 'version_to'",
                    ));
                }
                if field_attrs.optional && field_attrs.version_from.is_some() {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'optional' is not valid in combination with 'version_from'",
                    ));
                }
                if field_attrs.optional && field_attrs.version_to.is_some() {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'optional' is not valid in combination with 'version_to'",
                    ));
                }
                if field_attrs.version_from.is_some() && item_attrs.version == None {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'version_from' is requires 'version' to be defined on the struct",
                    ));
                }
                if field_attrs.version_to.is_some() && item_attrs.version == None {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'version_to' is requires 'version' to be defined on the struct",
                    ));
                }
                if field_attrs.skip && field_attrs.rename.is_some() {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'skip' is not valid in combination with 'rename'",
                    ));
                }

                let rename_field_ident = match field_attrs.rename {
                    Some(rename_field_ident) => rename_field_ident,
                    None => field_ident.clone(),
                };

                let field_decl_code = if field_attrs.skip {
                    quote! {
                        let #field_ident = <#field_ty>::default();
                    }
                } else if field_attrs.optional {
                    quote! {
                        let #field_ident = {
                            match node_fields.get(stringify!(#rename_field_ident).as_bytes()) {
                                Some(field_node) => {
                                    <#field_ty>::deserialize(field_node, allocator.clone())?
                                }
                                None => {
                                    <#field_ty>::default()
                                }
                            }
                        };
                    }
                } else if field_attrs.version_from == None && field_attrs.version_to == None {
                    // We could leave the unversioned field branch to runtime, but special-casing it
                    // here means we don't require the field to be default, because a value always
                    // has to be available.
                    quote! {
                        let #field_ident = {
                            let field_node = node_fields
                                .get(stringify!(#rename_field_ident).as_bytes())
                                .ok_or(wfserialize::DeserializeError::MissingStructField {
                                    ident: stringify!(#rename_field_ident),
                                })?;

                            <#field_ty>::deserialize(field_node, allocator.clone())?
                        };
                    }
                } else {
                    let field_version_from = if let Some(version_from) = field_attrs.version_from {
                        quote! { Some(#version_from) }
                    } else {
                        quote! { None }
                    };
                    let field_version_to = if let Some(version_to) = field_attrs.version_to {
                        quote! { Some(#version_to) }
                    } else {
                        quote! { None }
                    };

                    // We only handle version_from/version_to in this branch, because they are
                    // incompatible with skip and optional, which we check above.
                    quote! {
                        let #field_ident = {
                            let version_from = #field_version_from;
                            let version_to = #field_version_to;

                            // Based on version matching, the field is either mandatory or optional
                            // for deserialization.
                            let version_matches = if let &Some(version) = node_version {
                                match (version_from, version_to) {
                                    (Some(from), Some(to)) => version >= from && version <= to,
                                    (Some(from), None) => version >= from,
                                    (None, Some(to)) => version <= to,
                                    // TODO(jt): @Speed This could be marked
                                    // unreachable/unreachable_unchecked?
                                    (None, None) => true,
                                }
                            } else {
                                // This is pretty much unreachable, unless someone modifies the
                                // definition incorrectly after something was serialized. Instead of
                                // erroring, we can decide to make the field optional or
                                // mandatory. We pick mandatory to make the error visible sooner and
                                // prevent spreading of corrupted data.
                                true
                            };

                            if version_matches {
                                let field_node = node_fields
                                    .get(stringify!(#rename_field_ident).as_bytes())
                                    .ok_or(wfserialize::DeserializeError::MissingStructField {
                                        ident: stringify!(#rename_field_ident),
                                    })?;

                                <#field_ty>::deserialize(field_node, allocator.clone())?
                            } else {
                                match node_fields.get(stringify!(#rename_field_ident).as_bytes()) {
                                    Some(field_node) => {
                                        <#field_ty>::deserialize(field_node, allocator.clone())?
                                    }
                                    None => {
                                        <#field_ty>::default()
                                    }
                                }
                            }
                        };
                    }
                };

                let field_init_code = quote! {
                    #field_ident,
                };

                field_decl_codes.push(field_decl_code);
                field_init_codes.push(field_init_code);
            }

            // Don't even mention Migrate unless requested, as the impl might not exist.
            let init_and_maybe_migrate_code = if item_attrs.migrate {
                // We checked version is defined at the top of this function.
                let new_version = item_attrs.version.unwrap();
                quote! {
                    #(#field_decl_codes)*
                    let mut value = #ident { #(#field_init_codes)* };

                    wfserialize::Migrate::migrate(&mut value, *node_version, #new_version);

                    Ok(value)
                }
            } else {
                quote! {
                    #(#field_decl_codes)*
                    Ok(#ident { #(#field_init_codes)* })
                }
            };

            quote! {
                match node {
                    wfserialize::Node::DictionaryStruct(node_ident_bytes, node_version, node_fields) => {
                        let node_ident = wfserialize::cast_ident(&node_ident_bytes);
                        if stringify!(#rename_ident) == node_ident {
                            #init_and_maybe_migrate_code
                        } else {
                            Err(wfserialize::DeserializeError::Type {
                                expected: stringify!(#rename_ident),
                                // TODO(yan): This has to be extracted from the
                                // parse tree, and its lifetime has to be tied
                                // to the parse tree.
                                got: "<unavailable>",
                            })
                        }
                    }
                    other => Err(wfserialize::DeserializeError::Kind {
                        expected: wfserialize::Kind::DictionaryStruct,
                        got: other.kind(),
                    }),
                }
            }
        }
        syn::Fields::Unnamed(fields) => {
            let mut field_decl_codes: Vec<TokenStream2> = Vec::new();
            let mut field_init_codes: Vec<TokenStream2> = Vec::new();
            for (i, field) in fields.unnamed.iter().enumerate() {
                let field_ident = syn::Ident::new(&format!("__SERIALIZE__{i}"), Span::call_site());
                let field_ty = &field.ty;
                let field_attrs = parse_field_attrs(&field.attrs)?;

                if field_attrs.skip {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'skip' is not valid on unnamed struct fields",
                    ));
                }
                if field_attrs.optional {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'optional' is not valid on unnamed struct fields",
                    ));
                }
                if field_attrs.version_from.is_some() {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'version_from' is not valid on unnamed struct fields",
                    ));
                }
                if field_attrs.version_to.is_some() {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'version_to' is not valid on unnamed struct fields",
                    ));
                }
                if field_attrs.rename.is_some() {
                    return Err(syn::Error::new_spanned(
                        field,
                        "'rename' is not valid on unnamed struct fields",
                    ));
                }

                let field_decl_code = quote! {
                    let #field_ident = {
                        let field_node = node_fields
                            .get(#i)
                            .ok_or(wfserialize::DeserializeError::MissingStructField {
                                ident: stringify!(#i),
                            })?;
                        <#field_ty>::deserialize(field_node, allocator.clone())?
                    };
                };
                let field_init_code = quote! {
                    #field_ident,
                };

                field_decl_codes.push(field_decl_code);
                field_init_codes.push(field_init_code);
            }

            quote! {
                match node {
                    wfserialize::Node::TupleStruct(node_ident_bytes, node_fields) => {
                        let node_ident = wfserialize::cast_ident(&node_ident_bytes);
                        if stringify!(#rename_ident) == node_ident {
                            #(#field_decl_codes)*
                            Ok(#ident(#(#field_init_codes)*))
                        } else {
                            Err(wfserialize::DeserializeError::Type {
                                expected: stringify!(#rename_ident),
                                // TODO(yan): This has to be extracted from the
                                // parse tree, and its lifetime has to be tied
                                // to the parse tree.
                                got: "<unavailable>",
                            })
                        }
                    }
                    other => Err(wfserialize::DeserializeError::Kind {
                        expected: wfserialize::Kind::TupleStruct,
                        got: other.kind(),
                    }),
                }
            }
        }
        syn::Fields::Unit => {
            quote! {
                match node {
                    wfserialize::Node::UnitStruct(node_ident_bytes) => {
                        let node_ident = wfserialize::cast_ident(&node_ident_bytes);
                        if stringify!(#rename_ident) == node_ident {
                            Ok(#ident)
                        } else {
                            Err(wfserialize::DeserializeError::Type {
                                expected: stringify!(#rename_ident),
                                // TODO(yan): This has to be extracted from the
                                // parse tree, and its lifetime has to be tied
                                // to the parse tree.
                                got: "<unavailable>",
                            })
                        }
                    }
                    other => Err(wfserialize::DeserializeError::Kind {
                        expected: wfserialize::Kind::UnitStruct,
                        got: other.kind(),
                    }),
                }
            }
        }
    };

    Ok(quote! {
        const _: () = {
            impl #impl_generics wfserialize::Deserialize<#allocator_ty> for #ident #ty_generics #where_clause {
                fn deserialize<__SERIALIZE_NA__>(
                    node: &wfserialize::Node<__SERIALIZE_NA__>,
                    allocator: #allocator_ty,
                ) -> Result<Self, wfserialize::DeserializeError>
                where
                    __SERIALIZE_NA__: core::alloc::Allocator + core::clone::Clone,
                {
                    #deserialize_code
                }
            }
        };
    })
}

fn deserialize_enum(input: &syn::DeriveInput, data: &syn::DataEnum) -> syn::Result<TokenStream2> {
    let ident = &input.ident;
    let item_attrs = parse_item_attrs(&input.attrs)?;

    // Enums don't have their type name in the serialized text, so no rename here. They also don't
    // have versions or migration.
    if item_attrs.version.is_some() {
        return Err(syn::Error::new_spanned(input, "'version' is not valid on enums"));
    }
    if item_attrs.migrate {
        return Err(syn::Error::new_spanned(input, "'migrate' is not valid on enums"));
    }
    if item_attrs.rename.is_some() {
        return Err(syn::Error::new_spanned(input, "'rename' is not valid on enums"));
    }

    let (allocator_ty, generic) = match item_attrs.allocator {
        Some(allocator_ty) => (allocator_ty, false),
        None => {
            let allocator_ty = syn::Type::Path(syn::TypePath {
                qself: None,
                path: syn::Path {
                    leading_colon: None,
                    segments: {
                        let mut segments_punct = syn::punctuated::Punctuated::new();
                        segments_punct.push(syn::PathSegment {
                            ident: syn::Ident::new("__SERIALIZE_A__", Span::call_site()),
                            arguments: syn::PathArguments::None,
                        });

                        segments_punct
                    },
                },
            });

            (allocator_ty, true)
        }
    };

    // Unlike Serialize, the Deserialize trait has a generic parameter itself,
    // meaning we can't just cut-n-paste generics from the original type, but
    // instead we need to splice the generic parameter of the trait to the
    // impl_generics from the original type, but NOT the ty_generics or the
    // where_clause. We only need to do this, if the provided allocator
    // attribute is a generic type parameter and not a concrete type, and thus
    // needs to be present in the impl_generics.
    let mut impl_augmented_generics = input.generics.clone();

    // TODO(yan): Implement serialize(allocator_generic) for when the user provides
    // no concrete allocator, but wants to tie the allocator type parameter to
    // the allocator generic type defined on the struct.
    if generic {
        impl_augmented_generics
            .params
            .push(syn::GenericParam::Type(syn::TypeParam {
                attrs: Vec::new(),
                ident: syn::Ident::new("__SERIALIZE_A__", Span::call_site()),
                colon_token: Some(syn::token::Colon {
                    spans: [Span::call_site()],
                }),
                bounds: {
                    let mut bounds_punct = syn::punctuated::Punctuated::new();

                    bounds_punct.push(syn::TypeParamBound::Trait(syn::TraitBound {
                        paren_token: None,
                        modifier: syn::TraitBoundModifier::None,
                        lifetimes: None,
                        path: syn::Path {
                            leading_colon: None,
                            segments: {
                                let mut segments_punct = syn::punctuated::Punctuated::new();
                                segments_punct.push(syn::PathSegment {
                                    ident: syn::Ident::new("core", Span::call_site()),
                                    arguments: syn::PathArguments::None,
                                });
                                segments_punct.push(syn::PathSegment {
                                    ident: syn::Ident::new("alloc", Span::call_site()),
                                    arguments: syn::PathArguments::None,
                                });
                                segments_punct.push(syn::PathSegment {
                                    ident: syn::Ident::new("Allocator", Span::call_site()),
                                    arguments: syn::PathArguments::None,
                                });

                                segments_punct
                            },
                        },
                    }));

                    bounds_punct.push(syn::TypeParamBound::Trait(syn::TraitBound {
                        paren_token: None,
                        modifier: syn::TraitBoundModifier::None,
                        lifetimes: None,
                        path: syn::Path {
                            leading_colon: None,
                            segments: {
                                let mut segments_punct = syn::punctuated::Punctuated::new();
                                segments_punct.push(syn::PathSegment {
                                    ident: syn::Ident::new("core", Span::call_site()),
                                    arguments: syn::PathArguments::None,
                                });
                                segments_punct.push(syn::PathSegment {
                                    ident: syn::Ident::new("clone", Span::call_site()),
                                    arguments: syn::PathArguments::None,
                                });
                                segments_punct.push(syn::PathSegment {
                                    ident: syn::Ident::new("Clone", Span::call_site()),
                                    arguments: syn::PathArguments::None,
                                });

                                segments_punct
                            },
                        },
                    }));

                    bounds_punct
                },
                eq_token: None,
                default: None,
            }));
    }

    let (_, ty_generics, where_clause) = input.generics.split_for_impl();
    let (impl_generics, _, _) = impl_augmented_generics.split_for_impl();

    let mut match_arm_codes: Vec<TokenStream2> = Vec::new();
    for variant in &data.variants {
        let variant_ident = &variant.ident;

        let match_arm_code = match &variant.fields {
            syn::Fields::Named(fields) => {
                let mut field_decl_codes: Vec<TokenStream2> = Vec::new();
                let mut field_init_codes: Vec<TokenStream2> = Vec::new();
                for field in &fields.named {
                    let field_ident = field.ident.as_ref().unwrap();
                    let field_ty = &field.ty;
                    let field_attrs = parse_field_attrs(&field.attrs)?;

                    if field_attrs.skip {
                        return Err(syn::Error::new_spanned(field, "'skip' is not valid on enum fields"));
                    }
                    if field_attrs.optional {
                        return Err(syn::Error::new_spanned(field, "'optional' is not valid on enum fields"));
                    }
                    if field_attrs.version_from.is_some() {
                        return Err(syn::Error::new_spanned(
                            field,
                            "'version_from' is not valid on enum fields",
                        ));
                    }
                    if field_attrs.version_to.is_some() {
                        return Err(syn::Error::new_spanned(
                            field,
                            "'version_to' is not valid on enum fields",
                        ));
                    }
                    if field_attrs.rename.is_some() {
                        return Err(syn::Error::new_spanned(field, "'rename' is not valid on enum fields"));
                    }

                    let field_decl_code = quote! {
                        let #field_ident = {
                            let field_node = node_fields
                                .get(stringify!(#field_ident).as_bytes())
                                .ok_or(wfserialize::DeserializeError::MissingStructField {
                                    ident: stringify!(#field_ident),
                                })?;

                            <#field_ty>::deserialize(field_node, allocator.clone())?
                        };
                    };
                    let field_init_code = quote! {
                        #field_ident,
                    };

                    field_decl_codes.push(field_decl_code);
                    field_init_codes.push(field_init_code);
                }

                quote! {
                    wfserialize::Node::DictionaryStruct(node_ident_bytes, node_version, node_fields)
                        if wfserialize::cast_ident(&node_ident_bytes) == stringify!(#variant_ident) =>
                    {
                        if let &Some(version) = node_version {
                            return Err(wfserialize::DeserializeError::UnexpectedVersionInfo {
                                ident: stringify!(#variant_ident),
                                version,
                            });
                        }

                        #(#field_decl_codes)*
                        Ok(#ident::#variant_ident { #(#field_init_codes)* })
                    }
                }
            }
            syn::Fields::Unnamed(fields) => {
                let mut field_decl_codes: Vec<TokenStream2> = Vec::new();
                let mut field_init_codes: Vec<TokenStream2> = Vec::new();
                for (i, field) in fields.unnamed.iter().enumerate() {
                    let field_ident = syn::Ident::new(&format!("__SERIALIZE__{i}"), Span::call_site());
                    let field_ty = &field.ty;
                    let field_attrs = parse_field_attrs(&field.attrs)?;

                    if field_attrs.skip {
                        return Err(syn::Error::new_spanned(field, "'skip' is not valid on enum fields"));
                    }
                    if field_attrs.optional {
                        return Err(syn::Error::new_spanned(field, "'optional' is not valid on enum fields"));
                    }
                    if field_attrs.version_from.is_some() {
                        return Err(syn::Error::new_spanned(
                            field,
                            "'version_from' is not valid on enum fields",
                        ));
                    }
                    if field_attrs.version_to.is_some() {
                        return Err(syn::Error::new_spanned(
                            field,
                            "'version_to' is not valid on enum fields",
                        ));
                    }
                    if field_attrs.rename.is_some() {
                        return Err(syn::Error::new_spanned(field, "'rename' is not valid on enum fields"));
                    }

                    let field_decl_code = quote! {
                        let #field_ident = {
                            let field_node = node_fields
                                .get(#i)
                                .ok_or(wfserialize::DeserializeError::MissingStructField {
                                    ident: stringify!(#i),
                                })?;
                            <#field_ty>::deserialize(field_node, allocator.clone())?
                        };
                    };
                    let field_init_code = quote! {
                        #field_ident,
                    };

                    field_decl_codes.push(field_decl_code);
                    field_init_codes.push(field_init_code);
                }

                quote! {
                    wfserialize::Node::TupleStruct(node_ident_bytes, node_fields)
                        if wfserialize::cast_ident(&node_ident_bytes) == stringify!(#variant_ident) =>
                    {
                        #(#field_decl_codes)*
                        Ok(#ident::#variant_ident(#(#field_init_codes)*))
                    }
                }
            }
            syn::Fields::Unit => {
                quote! {
                    wfserialize::Node::UnitStruct(node_ident_bytes)
                        if wfserialize::cast_ident(&node_ident_bytes) == stringify!(#variant_ident) =>
                    {
                        Ok(#ident::#variant_ident)
                    }
                }
            }
        };

        match_arm_codes.push(match_arm_code);
    }

    Ok(quote! {
        const _: () = {
            impl #impl_generics wfserialize::Deserialize<#allocator_ty> for #ident #ty_generics #where_clause {
                fn deserialize<__SERIALIZE_NA__>(
                    node: &wfserialize::Node<__SERIALIZE_NA__>,
                    allocator: #allocator_ty,
                ) -> Result<Self, wfserialize::DeserializeError>
                where
                    __SERIALIZE_NA__: core::alloc::Allocator + core::clone::Clone,
                {
                    match node {
                        #(#match_arm_codes)*
                        // TODO(yan): What do we do for unexpected nodes when
                        // matching for enum variants? We could report a
                        // Type error if none of the name-guarded branches
                        // match (although the expected value is not just one,
                        // but any of the variant names), and a Kind error if
                        // this is not a struct kind at all, and couldn't
                        // possibly be an enum variant.
                        _ => Err(wfserialize::DeserializeError::Other),
                    }
                }
            }
        };
    })
}

#[derive(Default)]
struct SerializeItemAttrs {
    version: Option<u64>,
    migrate: bool,
    rename: Option<syn::Ident>,
    allocator: Option<syn::Type>,
}

fn parse_item_attrs(attrs: &[syn::Attribute]) -> syn::Result<SerializeItemAttrs> {
    let mut found_attrs = SerializeItemAttrs::default();

    for attr in attrs {
        if !attr.path().is_ident("serialize") {
            continue;
        }

        let list = match &attr.meta {
            syn::Meta::List(list) => list,
            other => {
                return Err(syn::Error::new_spanned(
                    other,
                    "unsupported attribute meta, 'serialize' must be a list",
                ));
            }
        };

        list.parse_nested_meta(|meta| {
            if meta.path.is_ident("version") {
                if found_attrs.version.is_some() {
                    return Err(meta.error("duplicate attribute 'serialize(version)"));
                }

                let value = meta.value()?;
                let lit: syn::LitInt = value.parse()?;
                let number: u64 = lit.base10_parse()?;

                found_attrs.version = Some(number);
            } else if meta.path.is_ident("migrate") {
                if found_attrs.migrate {
                    return Err(meta.error("duplicate attribute 'serialize(migrate)"));
                }

                found_attrs.migrate = true;
            } else if meta.path.is_ident("rename") {
                if found_attrs.rename.is_some() {
                    return Err(meta.error("duplicate attribute 'serialize(rename)"));
                }

                let value = meta.value()?;
                let lit: syn::LitStr = value.parse()?;
                let ident = syn::Ident::new(&lit.value(), lit.span());

                found_attrs.rename = Some(ident);
            } else if meta.path.is_ident("allocator") {
                if found_attrs.allocator.is_some() {
                    return Err(meta.error("duplicate attribute 'serialize(allocator)"));
                }

                let value = meta.value()?;
                let lit: syn::LitStr = value.parse()?;
                let ty = syn::parse_str::<syn::Type>(&lit.value())?;

                found_attrs.allocator = Some(ty);
            } else {
                return Err(meta.error("unrecognized attribute in 'serialize'"));
            }

            Ok(())
        })?;
    }

    Ok(found_attrs)
}

#[derive(Default)]
struct SerializeFieldAttrs {
    skip: bool,
    optional: bool,
    version_from: Option<u64>,
    version_to: Option<u64>,
    rename: Option<syn::Ident>,
}

fn parse_field_attrs(attrs: &[syn::Attribute]) -> syn::Result<SerializeFieldAttrs> {
    let mut found_attrs = SerializeFieldAttrs::default();

    for attr in attrs {
        if !attr.path().is_ident("serialize") {
            continue;
        }

        let list = match &attr.meta {
            syn::Meta::List(list) => list,
            other => {
                return Err(syn::Error::new_spanned(
                    other,
                    "unsupported attribute meta, 'serialize' must be a list",
                ));
            }
        };

        list.parse_nested_meta(|meta| {
            if meta.path.is_ident("skip") {
                if found_attrs.skip {
                    return Err(meta.error("duplicate attribute 'serialize(skip)"));
                }

                found_attrs.skip = true;
            } else if meta.path.is_ident("optional") {
                if found_attrs.optional {
                    return Err(meta.error("duplicate attribute 'serialize(optional)"));
                }

                found_attrs.optional = true;
            } else if meta.path.is_ident("version_from") {
                if found_attrs.version_from.is_some() {
                    return Err(meta.error("duplicate attribute 'serialize(version_from)"));
                }

                let value = meta.value()?;
                let lit: syn::LitInt = value.parse()?;
                let number: u64 = lit.base10_parse()?;

                found_attrs.version_from = Some(number);
            } else if meta.path.is_ident("version_to") {
                if found_attrs.version_to.is_some() {
                    return Err(meta.error("duplicate attribute 'serialize(version_to)"));
                }

                let value = meta.value()?;
                let lit: syn::LitInt = value.parse()?;
                let number: u64 = lit.base10_parse()?;

                found_attrs.version_to = Some(number);
            } else if meta.path.is_ident("rename") {
                if found_attrs.rename.is_some() {
                    return Err(meta.error("duplicate attribute 'serialize(rename)"));
                }

                let value = meta.value()?;
                let lit: syn::LitStr = value.parse()?;
                let ident = syn::Ident::new(&lit.value(), lit.span());

                found_attrs.rename = Some(ident);
            } else {
                return Err(meta.error("unrecognized attribute in 'serialize'"));
            }

            Ok(())
        })?;
    }

    Ok(found_attrs)
}
