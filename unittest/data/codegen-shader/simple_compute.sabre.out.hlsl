void main_main() {
	int x = 123;
}

[numthreads(1, 2, 3)]
void main()
{
	main_main();
}
