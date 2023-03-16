use dict::DictFelt252ToTrait;

trait WarpMemoryTrait {
    fn read_u128(ref self: DictFelt252To<u128>, loc: felt252) -> u128;
    fn read_u256(
        ref self: DictFelt252To<u128>, loc: felt252
    ) -> u256;
}

impl WarpMemoryImpl of WarpMemoryTrait {
	fn read_u128(ref self: DictFelt252To<u128>, loc: felt252) -> u128 {
		self.get(loc)
	}
	fn read_u256(
		ref self: DictFelt252To<u128>, loc: felt252
	) -> u256 {
		u256 { low: self.get(loc), high: self.get(loc + 1) }
	}
}