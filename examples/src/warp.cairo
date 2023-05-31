use starknet::ContractAddress;

#[contract]
mod WARP {
    use warplib::warp_memory::accessors::WarpMemoryAccessorTrait;
    use warplib::warp_memory::arrays::WarpMemoryArraysTrait;
    use warplib::warp_memory::WarpMemory;
    use warplib::warp_memory::WarpMemoryTrait;

    #[constructor]
    fn constructor() {
        return ();
    }

    #[view]
    fn init_static_array() {
        let mut warp_memory  = WarpMemoryTrait::initialize();

        wm0_static_array(1, 2);
        wm1_static_array(u256{low: 3, high: 0}, u256{low: 4, high: 8});
    }

    #[implicit(warp_memory: WarpMemory)]
    fn wm0_static_array(e0: u8, e1: u8) -> felt252 {
        let start = warp_memory.unsafe_alloc(2);

        //warp_memory.store(start, e0);
        //warp_memory.store(start + 1, e1);
        //start
        3
    }

    #[implicit(warp_memory: WarpMemory)]
    fn wm1_static_array(e0: u256, e1: u256) -> felt252 {
        //let start = warp_memory.unsafe_alloc(4);

        //warp_memory.store(start, e0.low);
        //warp_memory.store(start + 1, e0.high);
        //warp_memory.store(start + 2, e1.low);
        //warp_memory.store(start + 3, e1.high);
        //start
        3
    }
}

