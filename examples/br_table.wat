(module
    (func $main (param i32) (result i32)
        block
            block
                block
                    local.get 0
                    br_table 0 1 2 2
                end
                i32.const 0
                return
            end
            i32.const 1
            return
        end
        i32.const 2
    )
)
