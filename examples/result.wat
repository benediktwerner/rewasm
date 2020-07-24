(module
    (func (param i32 i32) (result i32)
        i32.const 1
        if (result i32)
            block (result i32)
                i32.const 15
            end
            br 0
            i32.const 2
        else
            i32.const 2
        end
        i32.const 324
        i32.add
        set_local 1
        block (result i32)
            get_local 1
            get_local 0
            br_if 0
            drop
            i32.const 1337
        end
    )
)
