(module
    (func (param i32 i32) (result i32)
        block
            block
                get_local 0
                br_if 0
                block
                    loop
                        get_local 0
                        i32.const 42
                        i32.mul
                        tee_local 0
                        br_if 1
                        get_local 0
                        i32.const 17
                        i32.add
                        br_if 2
                        br 0
                    end
                end
                i32.const 1337
                set_local 1
                br 1
            end
            i32.const 243
            set_local 1
        end
        get_local 1
        i32.const 1243
        i32.mul
    )
)
