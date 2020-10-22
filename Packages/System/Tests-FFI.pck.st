'From Cuis 5.0 [latest update: #4415] on 22 October 2020 at 9:09:44 pm'!
'Description '!
!provides: 'Tests-FFI' 1 0!
!requires: 'FFI' 1 33 nil!
SystemOrganization addCategory: 'Tests-FFI'!


!classDefinition: #FFIPluginTests category: 'Tests-FFI'!
TestCase subclass: #FFIPluginTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-FFI'!
!classDefinition: 'FFIPluginTests class' category: 'Tests-FFI'!
FFIPluginTests class
	instanceVariableNames: ''!

!classDefinition: #FFITestLibrary category: 'Tests-FFI'!
ExternalLibrary subclass: #FFITestLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-FFI'!
!classDefinition: 'FFITestLibrary class' category: 'Tests-FFI'!
FFITestLibrary class
	instanceVariableNames: ''!

!classDefinition: #FFISmallStruct1 category: 'Tests-FFI'!
ExternalStructure subclass: #FFISmallStruct1
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-FFI'!
!classDefinition: 'FFISmallStruct1 class' category: 'Tests-FFI'!
FFISmallStruct1 class
	instanceVariableNames: ''!

!classDefinition: #FFITestBiggerStruct category: 'Tests-FFI'!
ExternalStructure subclass: #FFITestBiggerStruct
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-FFI'!
!classDefinition: 'FFITestBiggerStruct class' category: 'Tests-FFI'!
FFITestBiggerStruct class
	instanceVariableNames: ''!

!classDefinition: #FFITestPoint2 category: 'Tests-FFI'!
ExternalStructure subclass: #FFITestPoint2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-FFI'!
!classDefinition: 'FFITestPoint2 class' category: 'Tests-FFI'!
FFITestPoint2 class
	instanceVariableNames: ''!

!classDefinition: #FFITestPoint4 category: 'Tests-FFI'!
ExternalStructure subclass: #FFITestPoint4
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-FFI'!
!classDefinition: 'FFITestPoint4 class' category: 'Tests-FFI'!
FFITestPoint4 class
	instanceVariableNames: ''!


!FFIPluginTests commentStamp: '<historical>' prior: 0!
SUnitized tests for the FFI (mostly the plugin side)!

!FFITestLibrary commentStamp: 'ar 8/14/2006 23:06' prior: 0!
ExternalLibrarty used in FFI tests!

!FFITestBiggerStruct commentStamp: '<historical>' prior: 0!
Used in FFI unit test(s)!

!FFITestPoint2 commentStamp: 'ar 8/14/2006 23:06' prior: 0!
A class used for testing structures as arguments for the FFI.!

!FFITestPoint4 commentStamp: 'ar 8/14/2006 23:06' prior: 0!
A class used for testing structures as arguments for the FFI.!

!FFIPluginTests methodsFor: 'external form tests' stamp: 'db 10/22/2020 01:37:34'!
testBlitToAndFromExternalForm
	"Ensure that we can blit to/from all 4 permutatations of Form and ExternalForm."
	| source external1 external2 destination |
	"Avoid translucency."
	source := (Cursor waitCursor asCursorForm asFormOfDepth: 1) asFormOfDepth: 32.
	destination := Form extent: source extent depth: 32.
	external1 := ExternalForm extent: source extent depth: 32.
	external1 allocateSpace.
	external2 := ExternalForm extent: source extent depth: 32.
	external2 allocateSpace.
	self shouldnt: [source bits = destination bits].
	source displayOn: external1.
	external1 displayOn: external2.
	external2 displayOn: destination.
	self should: [source bits = destination bits].! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 9/5/2016 21:13:26'!
testConstructedCharCall
	"Test using generic FFI spec"
	| result meth |
	meth := ExternalLibraryFunction
		name:'ffiTestChars' module: FFITestLibrary moduleName 
		callType: 0 returnType: ExternalType char
		argumentTypes: ((1 to: 4) collect:[:i| ExternalType char]).
	result := meth invokeWith: $A with: 65 with: 65.0 with: true.
	self assert: result class = Character.
	self assert: result numericValue = 130! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:22'!
testConstructedDoubleCall
	"Test using generic FFI spec"
	| result meth |
	meth := ExternalLibraryFunction
		name:'ffiTestDoubles' module: FFITestLibrary moduleName 
		callType: 0 returnType: ExternalType double
		argumentTypes: ((1 to: 2) collect:[:i| ExternalType double]).
	result := meth invokeWithArguments: (Array with: 41 with: true).
	self assert: result = 42.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:21'!
testConstructedFloatCall
	"Test using generic FFI spec"
	| result meth |
	meth := ExternalLibraryFunction
		name:'ffiTestFloats' module: FFITestLibrary moduleName 
		callType: 0 returnType: ExternalType float
		argumentTypes: ((1 to: 2) collect:[:i| ExternalType float]).
	result := meth invokeWith: $A with: 65.0.
	self assert: result = 130.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:20'!
testConstructedIntCall
	"Test using generic FFI spec"
	| result meth |
	meth := ExternalLibraryFunction
		name:'ffiTestInts' module: FFITestLibrary moduleName
		callType: 0 returnType: ExternalType long
		argumentTypes: ((1 to: 4) collect:[:i| ExternalType long]).
	result := meth invokeWith: $A with: 65 with: 65.0 with: true.
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:22'!
testConstructedPrintString
	"Test using generic FFI spec"
	| result meth |
	meth := ExternalLibraryFunction
		name:'ffiPrintString' module: FFITestLibrary moduleName 
		callType: 0 returnType: ExternalType string
		argumentTypes: ((1 to: 1) collect:[:i| ExternalType string]).
	result := meth invokeWith:'Hello World!!'.
	self assert: result = 'Hello World!!'.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:20'!
testConstructedShortCall
	"Test using generic FFI spec"
	| result meth |
	meth := ExternalLibraryFunction
		name:'ffiTestShorts' module: FFITestLibrary moduleName 
		callType: 0 returnType: ExternalType short
		argumentTypes: ((1 to: 4) collect:[:i| ExternalType short]).
	result := meth invokeWithArguments: (Array with: $A with: 65 with: 65.0 with: true).
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 9/5/2016 21:13:23'!
testGenericCharCall
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestChar: $A with: 65 with: 65.0 with: true.
	self assert: result class = Character.
	self assert: result numericValue = 130! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:17'!
testGenericDoubleCall
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestDoubles: $A with: 65.0.
	self assert: result = 130.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'djm 5/6/2014 02:52'!
testGenericDoubleCall2
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestDoubles9: 1.0 d: 2.0 d: 3.0 d: 4.0 d: 5.0 d: 6.0 d: 7.0 d: 8.0 d: 9.0.
	self assert: result = 45.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:17'!
testGenericFloatCall
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestFloats: $A with: 65.0.
	self assert: result = 130.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:17'!
testGenericIntCall
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestInt: $A with: 65 with: 65.0 with: true.
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 2/20/2019 00:15:10'!
testGenericMixedDoublesIntAndStruct
	"Test using generic FFI spec"
	| result int struct |
	int := 42.
	struct := FFITestPoint4 new.
	struct x: 1. struct y: 2. struct z: 3. struct w: 4.
	result := FFITestLibrary ffiTestMixedDoublesIntAndStruct: 1.0 d: 2.0 d: 3.0 d: 4.0 d: 5.0 d: 6.0 d: 7.0 d: 8.0 d: 9.0 i: int s: struct.
	self assert: result = (45.0 + 42 + 10)! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 12/17/2018 12:44:56'!
testGenericMixedFloatsAndDouble
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestMixedFloatsAndDouble: 1.2 with: 3.4 with: 5.6 with: 7.8.
	self assert: (1.2 + 3.4 + 5.6 + 7.8 - result) abs < 1.0e-6! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:17'!
testGenericPrintString
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiPrintString:'Hello World!!'.
	self assert: result = 'Hello World!!'.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:18'!
testGenericShortCall
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestShort: $A with: 65 with: 65.0 with: true.
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 9/5/2016 21:13:31'!
testLibraryCharCall
	"Test using call from ExternalLibrary"
	| result |
	result := FFITestLibrary new ffiTestChar: $A with: 65 with: 65.0 with: true.
	self assert: result class = Character.
	self assert: result numericValue = 130! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:17'!
testLibraryDoubleCall
	"Test using call from ExternalLibrary"
	| result |
	result := FFITestLibrary new ffiTestDoubles: $A with: 65.0.
	self assert: result = 130.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:17'!
testLibraryFloatCall
	"Test using call from ExternalLibrary"
	| result |
	result := FFITestLibrary new ffiTestFloats: $A with: 65.0.
	self assert: result = 130.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:17'!
testLibraryIntCall
	"Test using call from ExternalLibrary"
	| result |
	result := FFITestLibrary new ffiTestInt: $A with: 65 with: 65.0 with: true.
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 8/14/2006 19:17'!
testLibraryPrintString
	"Test using call from ExternalLibrary"
	| result |
	result := FFITestLibrary new ffiPrintString:'Hello World!!'.
	self assert: result = 'Hello World!!'.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'djm 5/6/2014 01:54'!
testLongLongA1
	"Test passing a char and two longlongs."
	| byte long1 long2 long3 |
	byte := 42.
	long1 := 16r123456789012.
	long2 := (-1 << 31).
	long3 := FFITestLibrary ffiTestLongLongA1: byte with: long1 with: long2.
	self assert: long3 = (byte + long1 + long2)! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'djm 5/6/2014 02:50'!
testLongLongA3
	"Test passing a char, a longlong, and another char."
	| byte1 long1 byte2 long2 |
	byte1 := 3.
	long1 := 16r123456789012.
	byte2 := 4.
	long2 := FFITestLibrary ffiTestLongLongA3: byte1 with: long1 with: byte2.
	self assert: long2 = (byte1 + long1 + byte2)! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'ar 7/28/2009 20:53'!
testUlongRange
	"Simple test for making sure the FFI can call certain numbers in the ulong range"
	| result |
	self shouldnt:[result := FFITestLibrary ffiTestUlong: 3103854339 with: 3103854339 with: 3103854339 with: 3103854339] raise: Error.
	self should: result =  1912741382.! !

!FFIPluginTests methodsFor: 'other tests' stamp: 'EstebanLorenzano 8/31/2015 11:24'!
testLoadSymbolFromModule
	| address |
	
	"Ok"
	address := ExternalAddress loadSymbol: 'ffiTestFloats' module: FFITestLibrary moduleName.
	self deny: address isNil.
	self deny: address isNull.
	"Symbol does not exists"
	self 
		should: [ ExternalAddress loadSymbol: 'ffi_does_not_exist' module: FFITestLibrary moduleName ]
		raise: Error.
	"Module does not exists"
	self 
		should: [ ExternalAddress loadSymbol: 'ffiTestFloats' module: 'NonExistingModule' ]
		raise: Error.		! !

!FFIPluginTests methodsFor: 'structure tests' stamp: 'KenD 2/4/2019 15:36:17'!
testBiggerStruct
	"Test passing and returning up of structures > 64 bit"
	| pt1 pt2 pt3 |
	pt1 := FFITestPoint4 new.
	pt1 x: 1. pt1 y: 2. pt1 z: 3. pt1 w: 4.
	pt2 := FFITestPoint4 new.
	pt2 x: 5. pt2 y: 6. pt2 z: 7. pt2 w: 8.
	pt3 := FFITestLibrary ffiTestBiggerStruct: pt1 with: pt2.
	self assert: pt3 x = 1.
	self assert: pt3 y = 2.
	self assert: pt3 z = 3.
	self assert: pt3 w = 4.
	self assert: pt3 r = 5.
	self assert: pt3 s = 6.
	self assert: pt3 t = 7.
	self assert: pt3 u = 8.! !

!FFIPluginTests methodsFor: 'structure tests' stamp: 'ar 8/14/2006 19:23'!
testLongLongs
	"Test passing and returning longlongs"
	| long1 long2 long3 |
	long1 := 16r123456789012.
	long2 := (-1 << 31).
	long3 := FFITestLibrary ffiTestLongLong: long1 with: long2.
	self assert: long3 = (long1 + long2)! !

!FFIPluginTests methodsFor: 'structure tests' stamp: 'djm 5/6/2014 14:55'!
testMixedIntAndStruct
	"Test passing an integer and two structures."
	| i1 pt1 pt2 result |
	i1 := 42.
	pt1 := FFITestPoint2 new.
	pt1 x: 3. pt1 y: 4.
	pt2 := FFITestPoint2 new.
	pt2 x: 5. pt2 y: 6.
	result := FFITestLibrary ffiTestMixedIntAndStruct: i1 with: pt1 with: pt2.
	self assert: result = 60.! !

!FFIPluginTests methodsFor: 'structure tests' stamp: 'djm 5/6/2014 15:45'!
testMixedIntAndStruct2
	"Test passing an integer and two structures."
	| i1 pt1 result |
	i1 := 42.
	pt1 := FFITestPoint4 new.
	pt1 x: 3. pt1 y: 4. pt1 z: 5. pt1 w: 6.
	result := FFITestLibrary ffiTestMixedIntAndStruct2: i1 with: pt1.
	self assert: result = 60.! !

!FFIPluginTests methodsFor: 'structure tests' stamp: 'djm 5/6/2014 16:02'!
testMixedIntAndStruct3
	"Test passing an integer and a small structure."
	| i1 pt1 result |
	i1 := 42.
	pt1 := FFISmallStruct1 new.
	pt1 x: 3. pt1 y: 4.
	result := FFITestLibrary ffiTestMixedIntAndStruct3: i1 with: pt1.
	self assert: result = 49! !

!FFIPluginTests methodsFor: 'structure tests' stamp: 'ar 8/14/2006 19:24'!
testPoint2
	"Test passing and returning up of structures >32bit and <= 64 bit"
	| pt1 pt2 pt3 |
	pt1 := FFITestPoint2 new.
	pt1 x: 1. pt1 y: 2.
	pt2 := FFITestPoint2 new.
	pt2 x: 3. pt2 y: 4.
	pt3 := FFITestLibrary ffiTestPoint2: pt1 with: pt2.
	self assert: pt3 x = 4.
	self assert: pt3 y = 6.! !

!FFIPluginTests methodsFor: 'structure tests' stamp: 'ar 8/14/2006 19:25'!
testPoint4
	"Test passing and returning up of structures > 64 bit"
	| pt1 pt2 pt3 |
	pt1 := FFITestPoint4 new.
	pt1 x: 1. pt1 y: 2. pt1 z: 3. pt1 w: 4.
	pt2 := FFITestPoint4 new.
	pt2 x: 5. pt2 y: 6. pt2 z: 7. pt2 w: 8.
	pt3 := FFITestLibrary ffiTestPoint4: pt1 with: pt2.
	self assert: pt3 x = 6.
	self assert: pt3 y = 8.
	self assert: pt3 z = 10.
	self assert: pt3 w = 12.! !

!FFIPluginTests methodsFor: 'structure tests' stamp: 'ar 8/14/2006 19:25'!
testPointers
	"Test passing and returning of pointers to structs"
	| pt1 pt2 pt3 |
	pt1 := FFITestPoint4 new.
	pt1 x: 1. pt1 y: 2. pt1 z: 3. pt1 w: 4.
	pt2 := FFITestPoint4 new.
	pt2 x: 5. pt2 y: 6. pt2 z: 7. pt2 w: 8.
	pt3 := FFITestLibrary ffiTestPointers: pt1 with: pt2.
	self assert: pt3 x = 6.
	self assert: pt3 y = 8.
	self assert: pt3 z = 10.
	self assert: pt3 w = 12.! !

!FFIPluginTests methodsFor: 'structure tests' stamp: 'djm 5/6/2014 05:26'!
testSmallStructureReturn
	"Test returning small structures (<4 bytes) which are returned in a register on some platforms."
	| pt1 |

	pt1 := FFITestLibrary ffiTestSmallStructReturn.
	self assert: pt1 x = 3.
	self assert: pt1 y = 4.! !

!FFITestLibrary methodsFor: 'primitives' stamp: 'ar 8/29/2011 21:59'!
ffiPrintString: aString
	"FFITestLibrary new ffiPrintString: 'Hello'"
	<cdecl: char* 'ffiPrintString' (char *)>
	^self externalCallFailed! !

!FFITestLibrary methodsFor: 'primitives' stamp: 'KenD 2/4/2019 15:27:30'!
ffiTestBiggerStruct: pt1 with: pt2
	<cdecl: FFITestBiggerStruct 'ffiTestStructBigger' (FFITestPoint4 FFITestPoint4) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary methodsFor: 'primitives' stamp: 'ar 8/29/2011 21:59'!
ffiTestChar: c1 with: c2 with: c3 with: c4
	"FFITestLibrary new ffiTestChar: $A with: 65 with: 65.0 with: true"
	<cdecl: char 'ffiTestChars' (char char char char)>
	^self externalCallFailed! !

!FFITestLibrary methodsFor: 'primitives' stamp: 'ar 8/29/2011 22:00'!
ffiTestDoubles: f1 with: f2
	"FFITestLibrary new ffiTestDoubles: $A with: 65.0"
	<cdecl: double 'ffiTestDoubles' (double double)>
	^self externalCallFailed! !

!FFITestLibrary methodsFor: 'primitives' stamp: 'ar 8/29/2011 22:00'!
ffiTestFloats: f1 with: f2
	"FFITestLibrary new ffiTestFloats: $A with: 65.0"
	<cdecl: float 'ffiTestFloats' (float float)>
	^self externalCallFailed! !

!FFITestLibrary methodsFor: 'primitives' stamp: 'ar 8/29/2011 22:00'!
ffiTestInt: c1 with: c2 with: c3 with: c4
	"FFITestLibrary new ffiTestInt: $A with: 65 with: 65.0 with: $A"
	<cdecl: long 'ffiTestInts' (long long long long)>
	^self externalCallFailed! !

!FFITestLibrary methodsFor: 'primitives' stamp: 'ar 8/29/2011 22:00'!
ffiTestShort: c1 with: c2 with: c3 with: c4
	"FFITestLibrary new ffiTestShort: $A with: 65 with: 65.0 with: $A"
	<cdecl: short 'ffiTestShorts' (short short short short)>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 8/14/2006 19:27'!
ffiPrintString: aString
	"FFITestLibrary ffiPrintString: 'Hello'"
	<cdecl: char* 'ffiPrintString' (char *) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'KenD 2/7/2019 02:52:17'!
ffiTestBiggerStruct: pt1 with: pt2
	<cdecl: FFITestBiggerStruct 'ffiTestStructBigger' (FFITestPoint4 FFITestPoint4) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 8/14/2006 19:27'!
ffiTestChar: c1 with: c2 with: c3 with: c4
	"FFITestLibrary ffiTestChar: $A with: 65 with: 65.0 with: true"
	<cdecl: char 'ffiTestChars' (char char char char) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'djm 5/6/2014 02:39'!
ffiTestDoubles9: f1 d: f2 d: f3 d: f4 d: f5 d: f6 d: f7 d: f8 d: f9
	"FFITestLibrary ffiTestDoubles9: 1.0 d: 2.0 d: 3.0 d: 4.0 d: 5.0 d: 6.0 d: 7.0 d: 8.0 d: 9.0"
	<cdecl: double 'ffiTestDoubles9' (double double double double double double double double double) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 8/14/2006 19:27'!
ffiTestDoubles: f1 with: f2
	"FFITestLibrary ffiTestDoubles: $A with: 65.0"
	<cdecl: double 'ffiTestDoubles' (double double) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 8/14/2006 19:27'!
ffiTestFloats: f1 with: f2
	"FFITestLibrary ffiTestFloats: $A with: 65.0"
	<cdecl: float 'ffiTestFloats' (float float) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 8/14/2006 19:28'!
ffiTestInt: c1 with: c2 with: c3 with: c4
	"FFITestLibrary ffiTestInt: $A with: 65 with: 65.0 with: true"
	<cdecl: long 'ffiTestInts' (long long long long) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 8/29/2011 22:01'!
ffiTestLongLong: long1 with: long2
	"FFITestLibrary ffiTestLongLong: 3 with: 4"
	<cdecl: longlong 'ffiTestLongLong' (longlong longlong) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'djm 5/6/2014 01:45'!
ffiTestLongLongA1: byte with: long1 with: long2
	"FFITestLibrary ffiTestLongLongA1: 3 with: 4 with: 5"
	<cdecl: longlong 'ffiTestLongLonga1' (char longlong longlong) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'djm 5/6/2014 02:22'!
ffiTestLongLongA3: byte1 with: long1 with: byte2
	"FFITestLibrary ffiTestLongLongA3: 3 with: 4 with: 5"
	<cdecl: longlong 'ffiTestLongLonga3' (char longlong char) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'djm 5/6/2014 17:20'!
ffiTestMixedDoublesIntAndStruct: f1 d: f2 d: f3 d: f4 d: f5 d: f6 d: f7 d: f8 d: f9 i: i1 s: s1
	"FFITestLibrary ffiTestMixedDoublesIntAndStruct: 1.0 d: 2.0 d: 3.0 d: 4.0 d: 5.0 d: 6.0 d: 7.0 d: 8.0 d: 9.0 i: 42
		s: (FFITestPoint4 new x: 3; y: 4; z: 5; w:6)"
	<cdecl: double 'ffiTestMixedDoublesIntAndStruct' (double double double double double double double double double long FFITestPoint4) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'djm 5/6/2014 05:08'!
ffiTestMixedFloatsAndDouble: f1 with: d1 with: f2 with: f3
	"FFITestLibrary ffiTestMixedFloatsAndDouble: 1.2 with: 3.4 with: 5.6 with: 7.8"
	<cdecl: double 'ffiTestMixedFloatsAndDouble' (float double float float) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'djm 5/6/2014 15:42'!
ffiTestMixedIntAndStruct2: i with: pt4
	"FFITestLibrary ffiTestMixedIntAndStruct2: 2 with: (FFITestPoint4 new x: 3; y: 4; z: 5; w:6)"
	<cdecl: long 'ffiTestMixedIntAndStruct2' (long FFITestPoint4) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'djm 5/6/2014 16:00'!
ffiTestMixedIntAndStruct3: i with: anFFISmallStruct1
	"FFITestLibrary ffiTestMixedIntAndStruct3: 2 with: (FFISmallStruct1 new x: 3; y: 4)"
	<cdecl: long 'ffiTestMixedIntAndStruct3' (long FFISmallStruct1) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'djm 5/6/2014 14:53'!
ffiTestMixedIntAndStruct: i with: pt1 with: pt2
	"FFITestLibrary ffiTestMixedIntAndStruct: 2 with: (FFITestPoint2 new x: 3; y: 4) with: (FFITestPoint2 new x: 5; y: 6)"
	<cdecl: long 'ffiTestMixedIntAndStruct' (long FFITestPoint2 FFITestPoint2) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 11/28/1999 19:51'!
ffiTestPoint2: pt1 with: pt2
	<cdecl: FFITestPoint2 'ffiTestStruct64' (FFITestPoint2 FFITestPoint2) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 11/28/1999 19:51'!
ffiTestPoint4: pt1 with: pt2
	<cdecl: FFITestPoint4 'ffiTestStructBig' (FFITestPoint4 FFITestPoint4) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 12/1/1999 16:39'!
ffiTestPointers: pt1 with: pt2
	<cdecl: FFITestPoint4* 'ffiTestPointers' (FFITestPoint4* FFITestPoint4*) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 8/14/2006 19:28'!
ffiTestShort: c1 with: c2 with: c3 with: c4
	"FFITestLibrary ffiTestShort: $A with: 65 with: 65.0 with:1"
	<cdecl: short 'ffiTestShorts' (short short short short) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'djm 5/6/2014 05:30'!
ffiTestSmallStructReturn
	"FFITestLibrary ffiTestSmallStructReturn"
	<cdecl: FFISmallStruct1 'ffiTestSmallStructReturn' (void) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'ar 7/28/2009 18:14'!
ffiTestUlong: c1 with: c2 with: c3 with: c4
	"FFITestLibrary ffiTestUlong: 3103854339 with: 3103854339 with: 3103854339 with: 3103854339"
	<cdecl: long 'ffiTestInts' (ulong ulong ulong ulong) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'accessing' stamp: 'ar 11/28/1999 19:51'!
moduleName
	"Use the fully qualified VM name so we ensure testing loading a library"
	^'SqueakFFIPrims'! !

!FFISmallStruct1 methodsFor: 'accessing' stamp: 'djm 5/6/2014 05:17'!
x
	"This method was automatically generated"
	^handle unsignedByteAt: 1! !

!FFISmallStruct1 methodsFor: 'accessing' stamp: 'djm 5/6/2014 05:17'!
x: anObject
	"This method was automatically generated"
	handle unsignedByteAt: 1 put: anObject! !

!FFISmallStruct1 methodsFor: 'accessing' stamp: 'djm 5/6/2014 05:17'!
y
	"This method was automatically generated"
	^handle unsignedByteAt: 2! !

!FFISmallStruct1 methodsFor: 'accessing' stamp: 'djm 5/6/2014 05:17'!
y: anObject
	"This method was automatically generated"
	handle unsignedByteAt: 2 put: anObject! !

!FFISmallStruct1 class methodsFor: 'as yet unclassified' stamp: 'djm 5/6/2014 05:17'!
fields
	"FFITestPoint2 defineFields"
	^#(
		(x	'byte')
		(y	'byte')
	)! !

!FFISmallStruct1 class methodsFor: 'class initialization' stamp: 'jmv 12/19/2016 15:55:57'!
initialize
	self compileFields! !

!FFITestBiggerStruct methodsFor: 'accessing'!
r
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	^handle signedLongLongAt: 33! !

!FFITestBiggerStruct methodsFor: 'accessing'!
r: anObject
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	handle signedLongLongAt: 33 put: anObject! !

!FFITestBiggerStruct methodsFor: 'accessing'!
s
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	^handle signedLongLongAt: 41! !

!FFITestBiggerStruct methodsFor: 'accessing'!
s: anObject
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	handle signedLongLongAt: 41 put: anObject! !

!FFITestBiggerStruct methodsFor: 'accessing'!
t
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	^handle signedLongLongAt: 49! !

!FFITestBiggerStruct methodsFor: 'accessing'!
t: anObject
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	handle signedLongLongAt: 49 put: anObject! !

!FFITestBiggerStruct methodsFor: 'accessing'!
u
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	^handle signedLongLongAt: 57! !

!FFITestBiggerStruct methodsFor: 'accessing'!
u: anObject
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	handle signedLongLongAt: 57 put: anObject! !

!FFITestBiggerStruct methodsFor: 'accessing'!
w
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	^handle signedLongLongAt: 25! !

!FFITestBiggerStruct methodsFor: 'accessing'!
w: anObject
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	handle signedLongLongAt: 25 put: anObject! !

!FFITestBiggerStruct methodsFor: 'accessing'!
x
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	^handle signedLongLongAt: 1! !

!FFITestBiggerStruct methodsFor: 'accessing'!
x: anObject
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	handle signedLongLongAt: 1 put: anObject! !

!FFITestBiggerStruct methodsFor: 'accessing'!
y
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	^handle signedLongLongAt: 9! !

!FFITestBiggerStruct methodsFor: 'accessing'!
y: anObject
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	handle signedLongLongAt: 9 put: anObject! !

!FFITestBiggerStruct methodsFor: 'accessing'!
z
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	^handle signedLongLongAt: 17! !

!FFITestBiggerStruct methodsFor: 'accessing'!
z: anObject
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	handle signedLongLongAt: 17 put: anObject! !

!FFITestBiggerStruct class methodsFor: 'field definition' stamp: 'KenD 2/7/2019 03:04:19'!
fields
	"FFITestPoint4 defineFields"
	^#(
		(x	'longlong')
		(y	'longlong')
		(z	'longlong')
		(w	'longlong')
		(r  'longlong')
		(s  'longlong')
		(t  'longlong')
		(u  'longlong')
	)! !

!FFITestBiggerStruct class methodsFor: 'class initialization' stamp: 'KenD 2/7/2019 03:04:32'!
initialize
"
	self defineFields; compileFields.
"
	self compileFields! !

!FFITestPoint2 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:42'!
x
	"This method was automatically generated"
	^handle signedLongAt: 1! !

!FFITestPoint2 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:42'!
x: anObject
	"This method was automatically generated"
	handle signedLongAt: 1 put: anObject! !

!FFITestPoint2 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:42'!
y
	"This method was automatically generated"
	^handle signedLongAt: 5! !

!FFITestPoint2 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:42'!
y: anObject
	"This method was automatically generated"
	handle signedLongAt: 5 put: anObject! !

!FFITestPoint2 class methodsFor: 'field definition' stamp: 'ar 12/1/1999 16:42'!
fields
	"FFITestPoint2 defineFields"
	^#(
		(x	'long')
		(y	'long')
	)! !

!FFITestPoint2 class methodsFor: 'class initialization' stamp: 'jmv 8/21/2013 17:39'!
initialize
	self compileFields! !

!FFITestPoint4 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:41'!
w
	"This method was automatically generated"
	^handle signedLongAt: 13! !

!FFITestPoint4 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:41'!
w: anObject
	"This method was automatically generated"
	handle signedLongAt: 13 put: anObject! !

!FFITestPoint4 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:41'!
x
	"This method was automatically generated"
	^handle signedLongAt: 1! !

!FFITestPoint4 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:41'!
x: anObject
	"This method was automatically generated"
	handle signedLongAt: 1 put: anObject! !

!FFITestPoint4 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:41'!
y
	"This method was automatically generated"
	^handle signedLongAt: 5! !

!FFITestPoint4 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:41'!
y: anObject
	"This method was automatically generated"
	handle signedLongAt: 5 put: anObject! !

!FFITestPoint4 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:41'!
z
	"This method was automatically generated"
	^handle signedLongAt: 9! !

!FFITestPoint4 methodsFor: 'accessing' stamp: 'ar 12/1/1999 16:41'!
z: anObject
	"This method was automatically generated"
	handle signedLongAt: 9 put: anObject! !

!FFITestPoint4 class methodsFor: 'field definition' stamp: 'ar 12/1/1999 16:41'!
fields
	"FFITestPoint4 defineFields"
	^#(
		(x	'long')
		(y	'long')
		(z	'long')
		(w	'long')
	)! !

!FFITestPoint4 class methodsFor: 'class initialization' stamp: 'jmv 8/21/2013 17:39'!
initialize
	self compileFields! !
FFISmallStruct1 initialize!
FFITestBiggerStruct initialize!
FFITestPoint2 initialize!
FFITestPoint4 initialize!
