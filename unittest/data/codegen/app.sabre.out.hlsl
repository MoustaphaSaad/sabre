int libA_x = 0;
int libA_y = 12;
int libA_foo() {
	return libA_y;
}

int libB_bar() {
	return libA_x + 1;
}

int main_x = 234;
int main_foo() {
	return libB_bar() + main_x + libA_x + libA_foo();
}
