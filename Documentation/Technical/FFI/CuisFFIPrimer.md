[_metadata_:author]:- "Eliot Miranda, Jon Raiford"
[_metadata_:last edited]:- "25 Nov 2024"
[_metadata_:viewer]:- "previewed using grip (homebrew install grip)"
# Cuis FFI Primer

Cuis has the ability to interoperate with foreign code that conforms to a given platform's Application Binary Interface (ABI) specification, essentially a codification of the C calling convention and data representation [^1].  FFI is an abbreviation of Foreign Function Interface. The Cuis FFI allows the programmer to

- load and unload dynamic libraries
- call functions defined in libraries
- pass a subset of Smalltalk objects as arguments to these functions, including objects that model or refer to external data
- pass as arguments to these functions callbacks that invoke Smalltalk blocks 
- refer to external data, including storing C structures in Smalltalk objects
- model complex C type definitions to make it easier to access external data
- insulate the programmer from as many details of the ABI as possible, and support the writing of portable cross-platform interfaces to foreign code

This document aims to convey the necessary information to make effective use of the Cuis FFI. It is not guaranteed to be comprehensive, and describes a moving target. Please use this document alongside the Cuis development environment. The curious programmer will find useful examples and by definition a complete FFI within Cuis itself.

[^1]: The ABI is sufficient to describe fully the C calling convention. Foreign code therefore is typically C libraries. C++ includes C as a subset, and therefore the C parts of C++ library APIs may be used directly. But C++ itself cannot be used directly; instead, define a suitable C interface using extern C { ... }, and create a library from that.

## The structure of this document is as follows:

  - [**Define Library Class**](#define-library-class)
  - [**Opening and Closing Dynamic Libraries**](#opening-and-closing-dynamic-libraries)
  - [**Defining Interface Methods to Library Functions**](#defining-interface-methods-to-library-functions)
    - [**Interface Method ExternalFunction Pragma Syntax**](#interface-method-externalfunction-pragma-syntax)
    - [**Example C Function Interface Method**](#example-c-function-interface-method)
    - [**Example Variadic C Function Interface Method**](#example-variadic-c-function-interface-method)
    - [**Using ExternalFunctions via invokeWith:...**](#using-externalfunctions)
  - [**Representing and Managing Data**](#representing-and-managing-data)
    - [**Primitive types**](#primitive-types)
    - [**Defining Structure types**](#defining-structure-types)
    - [**Garbage Collection, Object Lifetime, and Pinning**](#garbage-collection-object-lifetime-and-pinning)
    - [**Deprecated Argument Types**](#deprecated-argument-types)
  - [**Callbacks**](#callbacks)

## Define Library Class

In order to use a library, make a subclass of ExternalLibrary and implement the moduleName class method. Libraries should be accessed via singletons as they can only be opened once. The common pattern in Cuis is to use class instance variable named 'default' to reference this. If the interface is complex and has significant differences between 32 and 64 bit versions then consider creating an abstract class, MyLibrary, with two concrete subclasses, e.g. MyLibrary32 and MyLibrary64, with most code in the abstract superclass. Then have MyLibrary's new method test the word size and answer an instance of either MyLibrary32 or MyLibrary64 as appropriate.

<br>
ExternalLibrary subclass: #MyLibrary<br>
&emsp;instanceVariableNames: ''<br>
&emsp;classVariableNames: ''<br>
&emsp;poolDictionaries: ''<br>
&emsp;category: 'MyPackage-MyCategory'<br>
<br>
MyLibrary class<br>
&emsp;instanceVariableNames: 'default'<br><br>

*MyLibrary class methods for 'accessing'*<br>
**moduleName**<br>
&emsp;"Answer the name of the module for this library"<br>
&emsp;Smalltalk platformName = 'Win32' ifTrue:<br>
&emsp;&emsp;[^'mylib.dll'].<br>
&emsp;Smalltalk platformName = 'unix' ifTrue:<br>
&emsp;&emsp;[^'mylib.so'].<br>
&emsp;Smalltalk platformName = 'Mac OS' ifTrue:<br>
&emsp;&emsp;[^'mylib.dylib'].<br>
&emsp;^self error: 'Platform not supported'<br>

*MyLibrary class methods for 'instance creation'*<br>
**default**<br>
&emsp;*"Answer the library singleton (single class approach)"*<br>
&emsp;^default ifNil: [default := super new]!<br>

***or***<br>

**default**<br>
&emsp;*"Answer either a 32-bit or a 64-bit wrapper as appropriate (three class approach)."*<br>
&emsp;^default ifNil:<br>
&emsp;&emsp;&emsp;[default := (Smalltalk wordSize = 8 ifTrue: [MyLibrary64] ifFalse: [MyLibrary32])<br>
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;basicNew initialize]<br>

**new**<br>
&emsp;*"Prevent multiple instances"*<br>
&emsp;^self error: 'use #default'<br>

***or***<br>

**new**<br>
&emsp;*"Prevent multiple instances"*<br>
&emsp;^self default<br>

*MyLibrary class methods for 'system startup'*<br>
**install**<br>
&emsp;*"ExternalLibrary sends install to all ExternalLibrary subclasses on startup.<br>
&emsp; Use it to discard stale state. At least nil the default instance. All ExternalAddress<br>
&emsp; handles are invalidated on launching an image."*<br>
&emsp;default := nil<br>


## Opening and Closing Dynamic Libraries

Dynamic libraries are typically opened lazily when a function is first called. To force a library to load, send `#forceLoading` to the library singleton.
```
MyLibrary default forceLoading.
```
NOTE: Cuis does not currently have a method in the FFI package to close a library. One can use `Smalltalk unloadModule: <module name>`.[^2]
[^2]: An unload method could be added to the FFI package at short notice.

## Defining Interface Methods to Library Functions

There are two ways to invoke functions in libraries. The best way is to add an interface method to the library class for each function one wants to call. Each interface method should contain:
- Method Selector - There is no direct connection between the method selector and the function name, but it is a good idea to closely replicate the function name.[^3]
[^3]: Note that Cuis (and Squeak) support `_:` as a keyword, so it is possible to use selectors such as `strncmp:_:_:`, e.g. `libc strncmp: s1 _: s2 _: n.`
- Method Arguments - The names of the arguments do not matter other than they must be unique and begin with a lowercase letter
- (optional) Comment - It is a good idea to include the text of the function's C declaration in the method comment along with the description of the function
- Function Declaration - The function declaration is a pragma (a partial statement within angle brackets) with a keyword describing the calling convention, either `cdecl:` for the C calling convention, or `apicall:` for the Pascal calling convention, followed by the function's return type, the name of the function as a string, and a literal array (without leading `#`) of the types of the arguments. The full syntax is given below.
- Method Code - Code to run if the attempt to call the function fails. This is typically `^self externalCallFailed`

#### Interface Method ExternalFunction Pragma Syntax

An interface method pragma defining a foreign function is composed of the following sequence:
- begins with `<`
- calling convention keyword, either `cdecl:` for the caller-pops-arguments C calling convention, or `apicall:` for the callee-pops-arguments Pascal calling convention[^4]
[^4]: which for example is used in the WIN32 API where it is indicated by the __stdcall modifier.
- The function's return type#(argtype)
- The function's name as a string
- a left parenthesis `(`
- zero or more argument types#(argtype), one for each interface method argument/function argument pair, separated by whitespace. There is no checking beyond syntax. Chaos may ensue is you define too few arguments to a given function.[^5]
- one may include an "optional arguments" puncuator (```...```) at the relevant point. On most platforms ```...``` is unnecessary but on Apple ARMv8 (Apple Silicon) it is required for correct operation because there-on all optional arguments are passed on the stack and none in registers.
[^5]: Defining too many should be fine but is pointless and confusing.
- ends with `>`.

<a id="argtype"></a>
A return or argument type is a sequence of
- optional `const` keyword
- a type name, either one of the standard type names in the table below, or the name of a class inheriting from ExternalStructure that defines a C structure.
- optional whitespace
- an optional pointer indication, either `*` or `**`

#### Example C Function Interface Method

*ODBC3Library class methods for 'ODBC3-primitives'*<br>
**sqlBindParameter:** statementHandle<br>
**parameterNumber:** parameterNumber<br>
**inputOutputType:** inputOutputType<br>
**valueType:** valueType<br>
**parameterType:** parameterType<br>
**columnSize:** columnSize<br>
**decimalDigits:** decimalDigits<br>
**parameterValuePtr:** parameterValuePtr<br>
**bufferLength:** bufferLength<br>
**strLenOrIndPtr:** strLenOrIndPtr<br>
```
    "SQLRETURN SQLBindParameter(  
        SQLHSTMT        StatementHandle,  
        SQLUSMALLINT    ParameterNumber,  
        SQLSMALLINT     InputOutputType,  
        SQLSMALLINT     ValueType,  
        SQLSMALLINT     ParameterType,  
        SQLULEN         ColumnSize,  
        SQLSMALLINT     DecimalDigits,  
        SQLPOINTER      ParameterValuePtr,  
        SQLLEN          BufferLength,  
        SQLLEN *        StrLen_or_IndPtr);"

    <cdecl: int16 'SQLBindParameter' (SQLHSTMT uint16 int16 int16 int16 integer int16 void* int3264 SQLInteger*)>
    ^self externalCallFailed
```  
This function "SQLBindParameter" takes ten parameters and answers a SQLRETURN. It is necessary to look at the header file(s) to determine the atomic datatypes being used. In this case, SQLRETURN turns out to be a 16-bit integer. The first and last arguments for this function are passed as structures. StatementHandle is using the structure SQLHSTMT, which is defined as a subclass of ExternalStructure. Similarly, StrLen_or_IndPtr is defined as a SQLInteger* structure. The asterisk indicates that a pointer to the structure should be sent.

#### Example Variadic C Function Interface Method
*LibCLibrary methods for 'printing'*<br>
**printf:** format **with:** arg1 **with:** arg2
```
    "int printf(const char *format, ...);"

    <cdecl: int32 'printf' (char* ... uint3264 uint3264)>
    ^self externalCallFailed
```
<a id="using-externalfunctions"></a>
#### Using ExternalFunctions via invokeWith:...

Sometimes it is more convenient to refer to an external function via a variable, analogous to using function pointers in C, rather than through an interface method. Defining an interface method's pragma actually creates a hidden instance of ExternalLibraryFunction which is stored as the method's first literal (e.g. use "inspect method" on a library interface method to see it). The ExternalLibraryFunction includes data that is used by the SqueakFFIPrims plugin to marshall the arguments and return value from Smalltalk objects to C arguments, and from C return type to Smalltalk return value. Such objects can be created programmatically and used directly. ExternalLibraryFunction inherits from ExternalFunction. ExternalFunction can be used to wrap pointers to functions returned by foreign code, and invoke them. ExternalLibraryFunction can be used to refer to named functions in libraries. The function call mechanism is the same for both. As of this writing there is no direct support for instantiating ExternalFunction. Here is a realistic example of an ExternalLibraryFunction to invoke the C library's qsort function, `void qsort(void *base, size_t nel, size_t width, int (*compar)(const void *, const void *))` on a 64-bit platform.

```
	qsort := ExternalLibraryFunction
			name:'qsort'
			module: Alien libcName "the name of the platform's dynamic library for the standard C library"
			callType: 0 "0 is cdecl:, 1 is apicall:"
			returnType: ExternalType void
			argumentTypes: {ExternalType void asPointerType.
					ExternalType uint64.
					ExternalType uint64.
					ExternalType void asPointerType}.
```
ExternalFunctions are evaluated using invokeWith:[with:]*, e.g.
```
	qsort invokeWith: myDataElements with: nElements with: elementSize with: myCallback
```

### Representing and Managing Data
This section details
- how values are passed back and forth through interface methods
- how external data is referenced and how structure types are handled
- constraints the garbage collector imposes on the programmer

#### Primitive Types
The SqueakFFIPrims plugin understands a limited number of types; see FFIConstants class>>#initializeTypeConstants.[^6]
[^6]: in addition, the SqueakFFIPrims plugin has 16-bit and 32-bit Character types. These are useful for functions that return Character values. Passing Characters as parameters requires no special types since Characters are treated as unsigned integers by the argument marshalling machinery. **It may be useful to us to extent the FFI to allow the use of char16 and char32 return types.**

These are

| Data Type  | Description (32-bit Image)          | Description (64-bit Image)          |
|:-----------|:------------------------------------|:------------------------------------|
|  bool      |  32-bit Boolean                     |  64-bit Boolean                     |
|  char      |  8-bit Character (Unsigned)         |  8-bit Character (Unsigned)         |
| schar[^7]  |  8-bit Character (Signed)           |  8-bit Character (Signed)           |
|  float     | 4-byte Single precision float       | 4-byte Single precision float       |
|  double    | 8-byte Double precision float       | 8-byte Double precision float       |
| uint8      |  8-bit Integer (Unsigned)           |  8-bit Integer (Unsigned)           |
|  int8      |  8-bit Integer (Signed)             |  8-bit Integer (Signed)             |
| uint16     | 16-bit Integer (Unsigned)           | 16-bit Integer (Unsigned)           |
|  int16     | 16-bit Integer (Signed)             | 16-bit Integer (Signed)             |
| uint32     | 32-bit Integer (Unsigned)           | 32-bit Integer (Unsigned)           |
|  int32     | 32-bit Integer (Signed)             | 32-bit Integer (Signed)             |
| uint64     | 64-bit Integer (Unsigned)           | 64-bit Integer (Unsigned)           |
|  int64     | 64-bit Integer (Signed)             | 64-bit Integer (Signed)             |
| uint3264   | 32-bit Integer (Unsigned)           | 64-bit Integer (Unsigned)           |
|  int3264   | 32-bit Integer (Signed)             | 64-bit Integer (Signed)             |

[^7]: schar is a misnomer. Functions declared as returning schars answer Character instances that are fundamentally unsigned. As far as the SqueakFFIPrims plugin is concerned char and schar are the same type.

#### Deprecated Argument Types

The following argument types are considered deprecated. The newer, more explicit data types should be used instead.
- byte - Same as uint8
- sbyte - Same as int8
- schar - Same as char
- ushort - Same as uint16
- short - Same as int16
- ulong - Same as uint32
- long - Same as int32
- ulonglong - Same as uint64
- longlong - Same as int64
- size_t - Same as uint3264

The FFI (should??) redefines interface methods (and structure types) on start-up so that parameters of type uint3264 or int3264 are mapped into either uint64/int64 on 64-bit platforms or uint32/int32 on 32-bit platforms.

#### Parameter Passing

Several Smalltalk objects can be passed as actual parameters to formal parameters of the above types. In passing values through narrow parameters (e.g. passing SmallInteger maxVal through an int16) the value is first truncated, and then either zero-extended, if the type is unsigned, or sign-extended (from the most significant bit of the formal parameter's width, *not* the sign of the Smalltalk object) as dictated by the size and signedness of the formal parameter type. Any of these scalar types can be passed as either integer or floating point values, coerced according to C's integer/float implicit conversion rules (as applied in C assignment, cast, or return statements). N.B. There is *no* range checking beyond the plugin refusing to accept integers outside of the -2 ^ 63 to (2 ^ 64) - 1 range!![^8]
[^8]: e.g. if you pass 1234.5678 through a formal parameter of type uint8, it will be passed as `1234.5678 asInteger bitAnd: 16rFF`, which happens to be 210; if you pass -1234.5678 through a formal parameter of type int8, it will be passed as `(-1234.5678 asInteger bitOr: (-1 bitShift: 7))`, which happens to be -82.

- Integers in the range -2 ^ 63 to (2 ^ 64) - 1 (i.e. the union of the signed 64-bit 2's compliment and unsigned 64-bit ranges) are either zero-extended or sign extended 
- Floats[^9] are passed by value as implied by the introductory paragraph.
- `true` and `false` are mapped to 1 and 0, and zero extended to fill the width of the parameter (if integral, or passed as 1.0 and 0.0 if floating point)
- Character instances are mapped to their character code and zero extended to fill the width of the parameter (ditto).

[^9]: Cuis has only 64-bit Float objects obeying the IEEE 754-1985 standard (C's `double` type). But there are 32-bit float containers that convert between the external 64-bit and internal 32-bit representation, thereby saving space and providing compatibility with C's `float` type.

If a formal parameter is a pointer then as far as the SqueakFFIPrims plugin is concerned there are three cases.
- an instance of ExternalAddress is passed by value
- an instance of a raw bits object (String, Float32Array, Float32PointArray, Float64Array, IntegerArray, LargeNegativeInteger (!!), LargePositiveInteger (!!) et al; `(Smalltalk allClasses select: #isBits) reject: #isImmediateClass`) is passed as a pointer to the first byte of its data
- a general instance of ExternalStructure (an instance of a structure type) is passed as a pointer to the first byte of its data

If a formal parameter is a structure type then the actual parameter *must* be a general instance of ExternalStructure, and as many bytes as the sizeof the type of the formal parameter is passed. N.B. if the actual parameter is too small then garbage byets will be passed following the bytes of the actual parameter.[^10]
[^10]: The SqueakFFIPrims plugin can easily be extended to pass the memory pointed to by an ExternalAddress. Please inform Eliot if this is a pressing need.

#### Returning Results

The same set of objects are answered as values from interface methods whose return types are the same as the above.
- for bool the full word of the function's result is tested and if zero then false, otherwise true, is answered.
- for char and schar the least significant 8 bits of the function's result is answered as a Character[^6]
- for the integer types, the appropriate width of the result is collected, and then sign-extended if necessary, and answered as a suitable Integer instance.
- for the floating point types either the least significant 32-bits (for `float`) or the full 64-bits (for `double`) are answered as a Float instance (either BoxedFloat64 or SmallFloat64, as appropriate).

If the return type is pointer then
- if the type is char *, an instance of String (assumed to be ByteString) is answered (and the function result must be null-terminated)
- if the type is a pointer to a general instance of ExternalStructure, an instance of that class will be answered, whose data will be an ExternalAddress
- otherwise an instance of ExternalAddress will be answered

If the return type is a general instance of ExternalStructure, an instance of that class will be answered, whose data will be a ByteArray.

#### Garbage Collection, Object Lifetime, and Pinning
The OpenSmalltalk VM's Spur garbage collection scheme is the classical pairing of a generation scavenger for young objects with a mark-sweep collector for old objects. Most young objects are created in "eden", one of three small spaces, eden, past survivor space, and future survivor space, used to collect efficiently newly created objects. Large objects (64k words or larger) are created in old space. Objects living in new space are moved to old space when new space fills up with objects that survive; objects survive a scavenge when they are referenced from old objects. Objects living in new space are moved often, i.e. once every scavenge. Much less frequently old space is collected using the conventional recursive marking algorithm, and then compacted using a three finger algorithm that moves ordinary objects around "pinned" objects, leaving "pinned" objects where they are.

Given the Cuis VM's garbage collector moves objects, and the FFI supports callbacks, during which the garbage collector may run, and hence move objects, it is unsafe to pass reference parameters that live on the Cuis heap, unless the actual parameter has been pinned, or the function being called does not call-back. If and when the threaded FFI is available it will never be safe to pass objects that live on the Cuis heap unless they have been pinned, because Smalltalk execution can overlap with FFI callouts, and therefore objects may be moved during a callout.  It is always safe to pass data stored on the external C heap (general instances of ExternalData, things pointed to by general instances of ExternalAddress), provided this data is live.

The garbage collector will reclaim any and all unreferenced instances, including instances of ExternalAddress, et al, which will be finalized and hence the data they refer to will be freed back to the C heap. Therefore you ***must*** store objects somewhere (e.g. in instance variables of some object that is alive for as long as a library is used) whose data have been passed as actual parameters through interface methods. The garbage collector has no information on the lifetime of C data. It is your responsibility to keep objects used in the FFI safe from the garbage collector. You have been warned.

Objects may be "pinned" by sending them the "pin" message (which answers whether the object was already pinned, not the object itself).  If an object is in old space, this merely marks the object as pinned which has the effect of leaving the object where it is until it is collected (pinning an object does *not* prevent it from being garbage collected). Objects in new space which are pinned are first moved to old space. This is not a cheap operation, the cost being proportional to object size. Therefore it is a good idea to try and reuse pinned objects over the course of several FFI calls.
### Defining Structure types
Structure types are C's objects and it is important to be able to access fields within them, not just deal with them as blobs of data. The Cuis FFI provides the ExternalStructure hierarchy to do so. To define a structure type you create a subclass of ExternalStructure and fill in a class side method to define the fields and the field types of the structure. Structure fields can be other structures, but care must be taken when redefining a structure tyep used in another structure. Currently the containing structure is not automatically resized when a structure it contains is redefined.

Here is an example from the FFI_Tests package:
*FFITestBiggerStruct class methods for 'field definition'*<br>
**fields**<br>
&emsp;*"FFITestBiggerStruct defineFields"*<br>
&emsp;^#(<br>
&emsp;&emsp;(x 'int64_t')<br>
&emsp;&emsp;(y 'int64_t')<br>
&emsp;&emsp;(z 'int64_t')<br>
&emsp;&emsp;(w 'int64_t')<br>
&emsp;&emsp;(r 'int64_t')<br>
&emsp;&emsp;(s 'int64_t')<br>
&emsp;&emsp;(t 'int64_t')<br>
&emsp;&emsp;(u 'int64_t'))<br>

On evaluating `FFITestBiggerStruct defineFields` on a 64-bit platform the FFI compiler generates methods such as

*FFITestBiggerStruct methods for \*autogenerated*<br>
**x**<br>
&emsp;\<generated><br>
&emsp;^ handle int64At: 1<br>
<br>
**x:** t1 <br>
&emsp;\<generated><br>
&emsp;handle int64At: 1 put: t1<br>
<br>
This exemplifies structures, and pointers to structures, within structures:

*FFITestCompoundStruct class methods for 'field definition'*<br>
**fields**<br>
&emsp;"FFITestCompoundStruct defineFields"<br>
&emsp;^#(<br>
&emsp;&emsp;(s1 #FFISmallStruct1)<br>
&emsp;&emsp;(p1 'FFISmallStruct1 *')<br>
&emsp;&emsp;(s2 #FFITestPoint2)<br>
&emsp;&emsp;(p2 'FFITestPoint2 *')<br>
&emsp;&emsp;(s4 #FFITestPoint4)<br>
&emsp;&emsp;(p4 'FFITestPoint4 *')<br>
&emsp;)<br>

On evaluating `FFITestCompoundStruct defineFields` on a 64-bit platform the FFI compiler generates methods such as

*FFITestCompoundStruct methods for \*autogenerated*<br>
**s1**<br>
&emsp;\<generated><br>
&emsp;^ FFISmallStruct1 fromHandle: (handle structAt: 1 length: 2)<br>
<br>
**s1:** t1 <br>
&emsp;\<generated><br>
&emsp;handle structAt: 1 put: t1 getHandle length: 2<br>
<br>
**p1**<br>
&emsp;\<generated><br>
&emsp;^ FFISmallStruct1 fromHandle: (handle pointerAt: 9 length: 8)<br>
<br>
**p1:** t1 <br>
&emsp;\<generated><br>
&emsp;handle pointerAt: 9 put: t1 getHandle length: 8<br>

The handle of a general instance of ExternalStructure may be either a ByteArray, in which case its data exists on the Cuis heap, (and it may be wise to pin the handle), or an ExternalAddress, in which case its data exists on the C heap. In either case pay careful attention to [Garbage Collection, Object Lifetime, and Pinning](#garbage-collection-object-lifetime-and-pinning).

### Callbacks

The Cuis FFI supports callbacks which may be passed as actual parameters to foreign functions expecting them. The scheme maps a C callback to a Smalltalk block, with each parameter of the callback being passed to a subsequent block parameter. For example here is a Callback suitable for being passed to C's qsort routine:

```
	callback := Callback
			signature:  #(int (*)(const void *, const void *))
			block: [ :arg1 :arg2 |
				((arg1 doubleAt: 1) - (arg2 doubleAt: 1)) sign].
```

The type signature is in a different syntax to that used in interface methods.[^11]

To pass a Callback as an actual parameter in an interface method send one simply passes the Callback:[^12]
[^12]: older versions of the SqueakFFIPrims plugin required that one send thunk to the Callback
```
	libc qsort: myDoubles _: nDoubles _: ExternalType double byteSize _: callback thunk
```
To pull this off the FFI maintains a hierarchy of Callback classes, one for each ABI, which contain teh necessary marshalling methods. Currently there is no compiler from C function syntax to callback marshalling method. For the moment ask Eliot to write these methods by hand on an as-needed basis. See FFIPluginTests>>#testLibcQsort in the Tests-FFI package for a full example.
[^11]: the Callback type syntax is simply a  Smalltalk literal array. Igor Stasenko discovered that any C declaration can be expressed using literal array syntax, and this has the advantage that whitespace is ignored. Eliot Miranda likes the syntax and hopes to use it in interface method pragmas. If and when that is possible the FFI will either support two surface syntaxes or will provide tools to convert the old syntax to the new. An advantage of this syntax for interface methods is that now the interface method pragma is a normal pragma, pragmas being receiverless unary or keyword messages with literal arguments, which in practice is a very rich language for method annotations. *The potential implications for the metadata in LIMS methods should be noted.*

#### Footnotes
