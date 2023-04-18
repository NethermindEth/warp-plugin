#[contract]
mod ContractUsesImplicit {
    use warplib::memory::WarpMemory;
    use warplib::memory::WarpMemoryTrait;
    use warplib::memory::WarpMemoryImpl;

	#[implicit(warp_memory: WarpMemory)]
	fn call_to_insert(value: felt252) {
    	insert_to_warp_memory(value);
    }

	#[implicit(warp_memory: WarpMemory)]
	fn insert_to_warp_memory(value: felt252) {
		warp_memory.append(value);
	}
}
