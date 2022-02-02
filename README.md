# sabre

sabre is a shader language inspired by [golang](https://go.dev), it can target HLSL, GLSL, and SPIRV (is a work in progress)

## Simple Example

```golang
// comments start with '//'

// every file belong to a package, which means that it must start with a package declaration
package main

// packages can be imported using an import statements like so
// packages can be folders, or files
// std is a standard library implementation which is shipped with sabre
import "std"
// import statements can also have user define name other than the original package name like this
// now foo package is bound to bar symbol name
import bar "foo"

// structs are defined like so, they are just a list of fields with types
// VS_Input struct defines the input vertex layout to the vertex shader
type VS_Input struct {
	// fields are defined like so, they consist of name1, name2: type
	position: vec3,
}

// PS_Input struct defines vertex shader output, which is to be passed to pixel shader
type PS_Input struct {
	// struct fields can have tags, in this case '@system_position' tag indicates that this field contains the final/transformed vertex position.
	// GLSL equivalent is gl_Position, HLSL equivalent is SV_POSITION
	@system_position
	cs_position: vec4,
}

// another struct is defined to uniform buffer block type
type My_Uniform struct {
	mvp: mat4,
	// any other uniform fields can go here
}

// global variables are defined like so, var name: type;
// in this case this global variable has the '@uniform' tag which means it's a uniform buffer block
// tags can also have key value pairs, like the `binding = 1` which specifies a binding point for this uniform
// you can omit the explicit binding point, and sabre will choose one for you
@uniform{binding = 1}
var transform: My_Uniform;

// functions are defined like so, func name(arg1_name: arg1_type): return_type
// this function has '@vertex' tag which indicates that it can be vertex shader entry
@vertex
func vermeer_vertex(input: VS_Input): PS_Input {
	// in this statement we declare and define local variable, using composite literal expression
	// :type{field_name = value_expression}
	var output = :PS_Input{
		cs_position = transform.mvp * input.position
	};
	return output;
}

// this function has '@pixel' tag which indicates that it can be pixel shader entry
@pixel
func vermeer_pixel(input: PS_Input): PS_Output {
	// here we are using a composite literal as return value but we omitted the type, sabre compiler can infer the type from function return type
	return {
		color = {1, 0, 0, 1},
	};
}

// sabre also supports out of order declarations, so we can define this struct after using it in pixel shader entry
type PS_Output struct {
	color: vec4
}

// this is how constants are delcared, const name: type = value;
// this constant is being initialized with a composite literal value from the std package imported above
// it also has '@reflect' tag which means its value will be exported in the json description of this shader
@reflect
const pipeline = :std.Pipeline {
	rasterizer = { cull = false },
};
```

## Advanced Example
```golang
package main

// add is a generic function over T, it has two arguments both have type T, and it also returns T
func add<T: type>(a, b: T): T {
	return a + b;
}

// here we have a full specialization of the add function for the float type
func add(a, b: float): float {
	return a - b;
}

// structs can have generic arguments as well, here we define a generic point
// struct with a single generic argument
type Point struct<T: type> {
	x, y: T,
}

// here we have a specialization of the add function for the templated struct point, which will be favored when we call add on a `Point` type
func add<T: type>(a, b: Point<T>): Point<T> {
	return {
		a.x + b.x,
		a.y + b.y,
	};
}

// the compiler can infer the type of T based on the full type of TriangleStream, for example if you pass 'TriangleStream<int>` then the compiler will correctly infer T to be 'int'
func emit<T: type>(:TriangleStream<T>, :T) {
	// do something
}

// we have compile time if conditions, they are kind of like C #ifdef, but 'cleaner'
const dims = 4;

// compile time if conditions are just regular if conditions but done on the declaration level
if dims == 2 {
	type Point struct {
		p: vec2
	}
} else if dims == 3 {
	type Point struct {
		p: vec3
	}
} else {
	type Point struct {
		p: vec4
	}
}

// did i mention we have enums, they are useful
type Color_Mask enum {
	None = -1,
	Default = 0,
	Red = 1 << 0,
	Green = 1 << 1,
	Blue = 1 << 2,
	Alpha = 1 << 3,
	All = .Red | .Green | .Blue | .Alpha,
}
```