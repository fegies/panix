use proc_macro2::Span;
use quote::{ToTokens, quote, quote_spanned};
use syn::{Data, DeriveInput, Field, Variant, parse_macro_input, spanned::Spanned};

/// derive the content of the trace trait
#[proc_macro_derive(Trace)]
pub fn derive_tracable(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let body = generate_trace_body(&input.data);

    let expanded = quote!(
        unsafe impl #impl_generics gc::Trace for #name #ty_generics #where_clause {
            fn trace(&mut self, trace_fn: gc::TraceCallback) {
                #body
            }
        }
    );

    proc_macro::TokenStream::from(expanded)
}

fn generate_field_access(
    idx: usize,
    field: &Field,
    parent_expr: &proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let accessor = field
        .ident
        .as_ref()
        .map(|i| i.into_token_stream())
        .unwrap_or(syn::Index::from(idx).into_token_stream());
    quote_spanned! {field.span() => ::gc::Trace::trace(#parent_expr.#accessor, trace_fn);}
}

fn generate_variant_access(variant: &Variant) -> proc_macro2::TokenStream {
    let ident = &variant.ident;

    let idents = variant
        .fields
        .iter()
        .enumerate()
        .map(|(idx, f)| {
            f.ident
                .clone()
                .unwrap_or_else(|| syn::Ident::new(&format!("f{idx}"), Span::call_site()))
        })
        .collect::<Vec<_>>();

    let statements = idents.iter().map(|ident| quote!(#ident.trace(trace_fn);));

    let body = quote!({
        #(#statements)*
    });

    match variant.fields {
        syn::Fields::Named(_) => {
            quote!(Self::#ident{#(#idents,)*} => #body)
        }
        syn::Fields::Unnamed(_) => {
            quote!(Self::#ident(#(#idents,)*) => #body)
        }
        syn::Fields::Unit => quote!(Self::#ident => {}),
    }
}

fn generate_trace_body(data: &Data) -> proc_macro2::TokenStream {
    match data {
        Data::Struct(s) => {
            let self_expr = quote!(&mut self);
            let fields = s
                .fields
                .iter()
                .enumerate()
                .map(|(idx, f)| generate_field_access(idx, f, &self_expr));
            quote!(#(#fields)*)
        }
        Data::Enum(e) => {
            if e.variants.is_empty() {
                // in the case that there are no variants, we do not need to do anything.
                quote!()
            } else {
                let variants = e.variants.iter().map(generate_variant_access);
                let variants = quote!(#(#variants)*);
                quote!(match self {
                    #variants
                })
            }
        }
        Data::Union(_) => {
            panic!("C unions are not supported as there is no indication which variant is correct.")
        }
    }
}
