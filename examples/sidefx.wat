(module
    (func (param i32) (result i32)
        block
            block
                block
                    block
                        block
                            get_local 0
                            br_table 0 1 2
                        end
                        get_local 0
                        i32.const 42
                        i32.add
                        set_local 0
                        br 2
                    end
                    get_local 0
                    i32.const 17
                    i32.add
                    set_local 0
                    br 1
                end
                i32.const 1337
                set_local 0
                br 1
            end
            get_local 0
            i32.const 2
            i32.mul
            set_local 0
        end
        get_local 0
        i32.const 13
        i32.sub
        return
    )
)
