use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use rtic_syntax::ast::App;

use crate::{analyze::Analysis, check::Extra, codegen::util};

/// Generates code that runs before `#[init]`
pub fn codegen(app: &App, analysis: &Analysis, extra: &Extra) -> Vec<TokenStream2> {
    let mut stmts = vec![];

    let rt_err = util::rt_err_ident();

    // Disable interrupts -- `init` must run with interrupts disabled
    stmts.push(quote!(rtic::export::interrupt::disable();));

    // Populate the FreeQueue
    for (name, task) in &app.software_tasks {
        let cap = task.args.capacity;
        let fq_ident = util::fq_ident(name);

        stmts.push(quote!(
            (0..#cap).for_each(|i| (&mut *#fq_ident.get_mut()).enqueue_unchecked(i));
        ));
    }

    stmts.push(quote!(
        // To set the variable in cortex_m so the peripherals cannot be taken multiple times
        let mut core: rtic::export::Peripherals = rtic::export::Peripherals::steal().into();
    ));

    let device = &extra.device;
    let clic_prio_bits = quote!(#device::CLIC_PRIO_BITS);

    // Set number of bits used for interrupt level
    stmts.push(quote!(unsafe { core.CLIC.set_level_bit_width(#device::CLIC_PRIO_BITS.into()) }));

    // Set interrupt threshold to 0
    stmts.push(quote!(let mut mintthresh_reg = rtic::export::register::mintthresh::read();));
    stmts.push(quote!(mintthresh_reg.set_thresh(0);));
    stmts.push(quote!(rtic::export::register::mintthresh::write(mintthresh_reg);));

    // check that all dispatchers exists in the `Interrupt` enumeration regardless of whether
    // they are used or not
    let interrupt = util::interrupt_ident();
    for name in app.args.extern_interrupts.keys() {
        stmts.push(quote!(let _ = #rt_err::#interrupt::#name;));
    }

    let interrupt_ids = analysis.interrupts.iter().map(|(p, (id, _))| (p, id));

    // Unmask interrupts, set triggers, enable shv and set their priorities
    for (&priority, name) in interrupt_ids.chain(app.hardware_tasks.values().filter_map(|task| {
        if util::is_exception(&task.args.binds) {
            // We do exceptions in another pass
            None
        } else {
            Some((&task.args.priority, &task.args.binds))
        }
    })) {
        let es = format!(
            "Maximum priority used by interrupt vector '{}' is more than supported by hardware",
            name
        );
        // Compile time assert that this priority is supported by the device
        stmts.push(quote!(
            const _: () =  if (1 << #clic_prio_bits)-1 < #priority as usize { ::core::panic!(#es); };
        ));

        // Set priority
        stmts.push(quote!(
            core.CLIC.set_priority(
                #rt_err::#interrupt::#name,
                rtic::export::logical2hw(#priority, #clic_prio_bits),
            );
        ));

        // Enable hardware enabled vectoring
        if cfg!(feature="nxti"){
            stmts.push(quote!(core.CLIC.enable_shv(#rt_err::#interrupt::#name);));
        }

        // Set trigger to edge positive
        stmts.push(quote!(core.CLIC.set_trig(
            #rt_err::#interrupt::#name,
            rtic::export::peripheral::clic::Trigger::EdgePositive,
        );));

        // NOTE unmask the interrupt *after* setting its priority: changing the priority of a pended
        // interrupt is implementation defined
        stmts.push(quote!(rtic::export::CLIC::unmask(#rt_err::#interrupt::#name);));
    }

    // Set exception priorities
    for (name, priority) in app.hardware_tasks.values().filter_map(|task| {
        if util::is_exception(&task.args.binds) {
            Some((&task.args.binds, task.args.priority))
        } else {
            None
        }
    }) {
        let es = format!(
            "Maximum priority used by interrupt vector '{}' is more than supported by hardware",
            name
        );
        // Compile time assert that this priority is supported by the device
        stmts.push(quote!(
            const _: () =  if (1 << #clic_prio_bits)-1 < #priority as usize { ::core::panic!(#es); };
        ));

        stmts.push(quote!(core.CLIC.set_priority(
            rtic::export::SystemHandler::#name,
            rtic::export::logical2hw(#priority, #clic_prio_bits),
        );));
    }

    // Initialize monotonic's interrupts
    for (_, monotonic) in &app.monotonics {
        let priority = if let Some(prio) = monotonic.args.priority {
            quote! { #prio }
        } else {
            quote! { (((1_u32 << #clic_prio_bits)-1) as u8)}
        };
        let binds = &monotonic.args.binds;

        let name = &monotonic.ident;
        let es = format!(
            "Maximum priority used by monotonic '{}' is more than supported by hardware",
            name
        );
        // Compile time assert that this priority is supported by the device
        stmts.push(quote!(
            const _: () =  if (1 << #clic_prio_bits) < #priority as usize { ::core::panic!(#es); };
        ));

        let mono_type = &monotonic.ty;

        if &*binds.to_string() == "SysTick" {
            stmts.push(quote!(
                core.SCB.set_priority(
                    rtic::export::SystemHandler::SysTick,
                    rtic::export::logical2hw(#priority, #clic_prio_bits),
                );

                // Always enable monotonic interrupts if they should never be off
                if !<#mono_type as rtic::Monotonic>::DISABLE_INTERRUPT_ON_EMPTY_QUEUE {
                    core::mem::transmute::<_, rtic::export::SYST>(())
                        .enable_interrupt();
                }
            ));
        } else {

            if cfg!(feature="nxti"){
                stmts.push(quote!(
                    // Enable hardware enabled vectoring
                    core.CLIC.enable_shv(#rt_err::#interrupt::#binds);
                ))
            }

            stmts.push(quote!(
                core.CLIC.set_priority(
                    #rt_err::#interrupt::#binds,
                    rtic::export::logical2hw(#priority, #clic_prio_bits),
                );

                // Set trigger to edge positive
                core.CLIC.set_trig(
                    #rt_err::#interrupt::#binds,
                    rtic::export::peripheral::clic::Trigger::EdgePositive,
                );

                // Always enable monotonic interrupts if they should never be off
                if !<#mono_type as rtic::Monotonic>::DISABLE_INTERRUPT_ON_EMPTY_QUEUE {
                    rtic::export::CLIC::unmask(#rt_err::#interrupt::#binds);
                }
            ));
        }
    }
    stmts
}
