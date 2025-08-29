extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use wfcommon::cast_u32;

// TODO(yan): Add struct attr repr(int) for structs containing a single int
// field.

// TODO(yan): Add struct attr repr(float) for structs containing a single float
// field.

// TODO(yan): Add struct attrs repr(ivec2), repr(ivec3) and repr(ivec4). These
// would work on int arrays and int math vectors.

// TODO(yan): Add struct attrs repr(vec2), repr(vec3) and repr(vec4). These
// would work on float arrays and float math vectors.

// TODO(yan): Add struct attr repr(color). This would work on int and float
// arrays of length 3 and 4, as well as math vectors of length 3 and 4.

#[proc_macro_derive(Edit, attributes(edit))]
pub fn derive_edit(input: TokenStream) -> TokenStream {
    let parsed_input = syn::parse_macro_input!(input as syn::DeriveInput);
    edit(parsed_input).unwrap_or_else(|err| err.to_compile_error()).into()
}

fn edit(input: syn::DeriveInput) -> syn::Result<TokenStream2> {
    match &input.data {
        syn::Data::Struct(data) => edit_struct(&input, data),
        syn::Data::Enum(data) => edit_enum(&input, data),
        syn::Data::Union(_) => Err(syn::Error::new(Span::call_site(), "Unions are not supported, Dave")),
    }
}

fn edit_struct(input: &syn::DeriveInput, data: &syn::DataStruct) -> syn::Result<TokenStream2> {
    let ident = &input.ident;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut field_codes: Vec<TokenStream2> = Vec::new();
    match &data.fields {
        syn::Fields::Named(fields) => {
            for (i, field) in fields.named.iter().enumerate() {
                let field_ident = field.ident.as_ref().unwrap();
                let field_id = cast_u32(i);
                let field_attrs = parse_field_attrs(&field.attrs)?;
                let field_readonly = field_attrs.readonly;

                if !field_attrs.skip {
                    let field_code = quote! {
                        modified |= self.#field_ident.edit(
                            frame,
                            wfgui::id!(#field_id),
                            concat!(stringify!(#field_ident), ":"),
                            readonly || #field_readonly,
                        );
                    };

                    field_codes.push(field_code);
                }
            }
        }
        syn::Fields::Unnamed(fields) => {
            for (i, field) in fields.unnamed.iter().enumerate() {
                let field_index = syn::Index::from(i);
                let field_id = cast_u32(i);
                let field_attrs = parse_field_attrs(&field.attrs)?;
                let field_readonly = field_attrs.readonly;

                if !field_attrs.skip {
                    let field_code = quote! {
                        modified |= self.#field_index.edit(
                            frame,
                            wfgui::id!(#field_id),
                            concat!(#i, ":"),
                            readonly || #field_readonly,
                        );
                    };

                    field_codes.push(field_code);
                }
            }
        }
        syn::Fields::Unit => (),
    }

    Ok(quote! {
        const _: () = {
            impl #impl_generics wfedit::Edit for #ident #ty_generics #where_clause {
                fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
                    let mut modified = false;

                    if let Some(panel) = wfgui::begin_panel_with_fit_height(frame, id, "100%", label) {
                        #(#field_codes)*

                        panel.end(frame);
                    }

                    modified
                }
            }
        };
    })
}

fn edit_enum(input: &syn::DeriveInput, data: &syn::DataEnum) -> syn::Result<TokenStream2> {
    let ident = &input.ident;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut name_codes: Vec<TokenStream2> = Vec::new();
    let mut edit_match_arm_codes: Vec<TokenStream2> = Vec::new();
    let mut index_match_arm_codes: Vec<TokenStream2> = Vec::new();
    let mut init_match_arm_codes: Vec<TokenStream2> = Vec::new();

    for (variant_index, variant) in data.variants.iter().enumerate() {
        let variant_ident = &variant.ident;

        let (edit_code, index_code, init_code) = match &variant.fields {
            syn::Fields::Named(fields) => {
                let mut destructuring_codes: Vec<TokenStream2> = Vec::new();
                let mut field_codes: Vec<TokenStream2> = Vec::new();
                let mut field_init_codes: Vec<TokenStream2> = Vec::new();

                for (i, field) in fields.named.iter().enumerate() {
                    let field_ident = field.ident.as_ref().unwrap();
                    let field_ty = &field.ty;
                    let field_id = cast_u32(i);
                    let field_attrs = parse_field_attrs(&field.attrs)?;
                    let field_readonly = field_attrs.readonly;

                    let destructuring_code = quote! {
                        #field_ident,
                    };
                    let field_code = quote! {
                        modified |= #field_ident.edit(
                            frame,
                            wfgui::id!(#field_id),
                            concat!(stringify!(#field_ident), ":"),
                            readonly || #field_readonly,
                        );
                    };

                    let field_init_code = quote! {
                        #field_ident: <#field_ty>::default(),
                    };

                    if !field_attrs.skip {
                        destructuring_codes.push(destructuring_code);
                        field_codes.push(field_code);
                    }

                    field_init_codes.push(field_init_code);
                }

                let edit_code = quote! {
                    #ident::#variant_ident { #(#destructuring_codes)* } => {
                        #(#field_codes)*
                    }
                };

                let index_code = quote! {
                    #ident::#variant_ident { .. } => Some(#variant_index),
                };

                let init_code = quote! {
                    #variant_index => #ident::#variant_ident { #(#field_init_codes)* },
                };

                (edit_code, index_code, init_code)
            }
            syn::Fields::Unnamed(fields) => {
                let mut destructuring_codes: Vec<TokenStream2> = Vec::new();
                let mut field_codes: Vec<TokenStream2> = Vec::new();
                let mut field_init_codes: Vec<TokenStream2> = Vec::new();

                for (i, field) in fields.unnamed.iter().enumerate() {
                    let field_ident = syn::Ident::new(&format!("__EDIT__{i}"), Span::call_site());
                    let field_ty = &field.ty;
                    let field_id = cast_u32(i);
                    let field_attrs = parse_field_attrs(&field.attrs)?;
                    let field_readonly = field_attrs.readonly;

                    let destructuring_code = quote! {
                        #field_ident,
                    };
                    let field_code = quote! {
                        modified |= #field_ident.edit(
                            frame,
                            wfgui::id!(#field_id),
                            concat!(#i, ":"),
                            readonly || #field_readonly,
                        );
                    };
                    let field_init_code = quote! {
                        <#field_ty>::default(),
                    };

                    if !field_attrs.skip {
                        destructuring_codes.push(destructuring_code);
                        field_codes.push(field_code);
                    }

                    field_init_codes.push(field_init_code);
                }

                let edit_code = quote! {
                    #ident::#variant_ident( #(#destructuring_codes)* ) => {
                        #(#field_codes)*
                    }
                };

                let index_code = quote! {
                    #ident::#variant_ident(..) => Some(#variant_index),
                };

                let init_code = quote! {
                    #variant_index => #ident::#variant_ident( #(#field_init_codes)* ),
                };

                (edit_code, index_code, init_code)
            }
            syn::Fields::Unit => {
                let edit_code = quote! {
                    #ident::#variant_ident => {}
                };

                let index_code = quote! {
                    #ident::#variant_ident => Some(#variant_index),
                };

                let init_code = quote! {
                    #variant_index => #ident::#variant_ident,
                };

                (edit_code, index_code, init_code)
            }
        };

        let name_code = quote! {
            stringify!(#variant_ident),
        };

        name_codes.push(name_code);
        edit_match_arm_codes.push(edit_code);
        index_match_arm_codes.push(index_code);
        init_match_arm_codes.push(init_code);
    }

    Ok(quote! {
        const _: () = {
            impl #impl_generics wfedit::Edit for #ident #ty_generics #where_clause {
                fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
                    let mut modified = false;

                    if let Some(panel) = wfgui::begin_panel_with_fit_height(frame, id, "100%", label) {
                        {
                            static NAMES: &[&str] = &[ #(#name_codes)* ];

                            let mut selected_name_index = match self {
                                #(#index_match_arm_codes)*
                            };
                            let selected_name_index_orig = selected_name_index;

                            modified |= wfgui::dropdown(
                                frame,
                                // Fields start their IDs from 0 and grow upwards, so
                                // the discriminant gets the max value.
                                wfgui::id!(u32::MAX),
                                concat!(stringify!(#ident), ":"),
                                NAMES,
                                &mut selected_name_index,
                            );

                            if selected_name_index != selected_name_index_orig {
                                if let Some(name_index) = selected_name_index {
                                    *self = match name_index {
                                        #(#init_match_arm_codes)*
                                        _ => unreachable!("This macro has seriously miscalculated"),
                                    }
                                }
                            }
                        }

                        match self {
                            #(#edit_match_arm_codes)*
                        }

                        panel.end(frame);
                    }

                    modified
                }
            }
        };
    })
}

#[derive(Default)]
struct EditFieldAttrs {
    skip: bool,
    readonly: bool,
}

fn parse_field_attrs(attrs: &[syn::Attribute]) -> syn::Result<EditFieldAttrs> {
    let mut found_attrs = EditFieldAttrs::default();

    for attr in attrs {
        if !attr.path().is_ident("edit") {
            continue;
        }

        let list = match &attr.meta {
            syn::Meta::List(list) => list,
            other => {
                return Err(syn::Error::new_spanned(
                    other,
                    "unsupported attribute meta, 'edit' must be a list",
                ));
            }
        };

        list.parse_nested_meta(|meta| {
            if meta.path.is_ident("skip") {
                if found_attrs.skip {
                    return Err(meta.error("duplicate attribute 'edit(skip)"));
                }

                found_attrs.skip = true;
            } else if meta.path.is_ident("readonly") {
                if found_attrs.readonly {
                    return Err(meta.error("duplicate attribute 'edit(readonly)"));
                }

                found_attrs.readonly = true;
            } else {
                return Err(meta.error("unrecognized attribute in 'edit'"));
            }

            Ok(())
        })?;
    }

    Ok(found_attrs)
}
