int main_flat_octree_child_of(int i) {
	return 8 * i + 1;
}
struct main_Node {
	int3 min;
	int3 max;
};
struct main_Octree {
	main_Node nodes[1];
};
ByteAddressBuffer main_tree: register(t0);
void main_traverse() {
	int index = 0;
	{ // for scope
		// for init statement
		int d = 0;
		[loop]
		while (d < 4) {
			// for body
			int first_child_index = main_flat_octree_child_of(index);
			{ // for scope
				// for init statement
				int i = 0;
				[loop]
				while (i < 8) {
					// for body
					main_Node _tmp_1;
					int3 _tmp_2 = (main_tree.Load3(0));
					_tmp_1.min = _tmp_2;
					int3 _tmp_3 = (main_tree.Load3(16));
					_tmp_1.max = _tmp_3;
					main_Node node = _tmp_1;
					// for post statement
					++i;
				}
			} // for scope
			// for post statement
			++d;
		}
	} // for scope
}
void main_main() {
	main_traverse();
}

[numthreads(100, 1, 1)]
void main()
{
	main_main();
}
