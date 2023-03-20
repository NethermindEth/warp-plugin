#[contract]
mod ContractUsesImplicit {
	#[implicit(warp_memory)]
	fn read_from_memory(index: felt252) {
		warp_memory.read_u128(index);
	}

	#[implicit(warp_memory)]
	fn read_from_memory_2(index: felt252) {
    		warp_memory.read_u128(index);
    	}
}