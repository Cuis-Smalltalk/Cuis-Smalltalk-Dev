'From Cuis 6.0 [latest update: #6070] on 14 November 2023 at 2:27:43 pm'!
'Description '!
!provides: 'Tests-FFI' 1 3!
!requires: 'FFI' 1 39 nil!
SystemOrganization addCategory: #'Tests-FFI'!


!classDefinition: #FFIPluginTests category: #'Tests-FFI'!
TestCase subclass: #FFIPluginTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-FFI'!
!classDefinition: 'FFIPluginTests class' category: #'Tests-FFI'!
FFIPluginTests class
	instanceVariableNames: ''!

!classDefinition: #FFITestLibrary category: #'Tests-FFI'!
ExternalLibrary subclass: #FFITestLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-FFI'!
!classDefinition: 'FFITestLibrary class' category: #'Tests-FFI'!
FFITestLibrary class
	instanceVariableNames: ''!

!classDefinition: #FFISmallStruct1 category: #'Tests-FFI'!
ExternalStructure subclass: #FFISmallStruct1
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-FFI'!
!classDefinition: 'FFISmallStruct1 class' category: #'Tests-FFI'!
FFISmallStruct1 class
	instanceVariableNames: ''!

!classDefinition: #FFITestBiggerStruct category: #'Tests-FFI'!
ExternalStructure subclass: #FFITestBiggerStruct
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-FFI'!
!classDefinition: 'FFITestBiggerStruct class' category: #'Tests-FFI'!
FFITestBiggerStruct class
	instanceVariableNames: ''!

!classDefinition: #FFITestPoint2 category: #'Tests-FFI'!
ExternalStructure subclass: #FFITestPoint2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-FFI'!
!classDefinition: 'FFITestPoint2 class' category: #'Tests-FFI'!
FFITestPoint2 class
	instanceVariableNames: ''!

!classDefinition: #FFITestPoint4 category: #'Tests-FFI'!
ExternalStructure subclass: #FFITestPoint4
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-FFI'!
!classDefinition: 'FFITestPoint4 class' category: #'Tests-FFI'!
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

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 11/14/2023 10:38:50'!
testConstructedInt16Call
	"Test using generic FFI spec"
	| result meth |
	meth := ExternalLibraryFunction
		name:'ffiTestShorts' module: FFITestLibrary moduleName 
		callType: 0 returnType: ExternalType int16
		argumentTypes: ((1 to: 4) collect:[:i| ExternalType int16]).
	result := meth invokeWithArguments: (Array with: $A with: 65 with: 65.0 with: true).
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 11/14/2023 10:39:01'!
testConstructedInt32Call
	"Test using generic FFI spec"
	| result meth |
	meth := ExternalLibraryFunction
		name:'ffiTestInts' module: FFITestLibrary moduleName
		callType: 0 returnType: ExternalType int32
		argumentTypes: ((1 to: 4) collect:[:i| ExternalType int32]).
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

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 1/6/2023 10:36:34'!
testGenericCharCall
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestChar: $A asCharacter with: 65 with: 65.0 with: true.
	self assert: result class = Character.
	self assert: result numericValue = 130! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 1/6/2023 10:55:08'!
testGenericDoubleCall
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestDoubles: $A asCharacter with: 65.0.
	self assert: result = 130.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'djm 5/6/2014 02:52'!
testGenericDoubleCall2
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestDoubles9: 1.0 d: 2.0 d: 3.0 d: 4.0 d: 5.0 d: 6.0 d: 7.0 d: 8.0 d: 9.0.
	self assert: result = 45.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 1/6/2023 10:55:25'!
testGenericFloatCall
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestFloats: $A asCharacter with: 65.0.
	self assert: result = 130.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 11/14/2023 10:36:38'!
testGenericInt16Call
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestInt16: $A asCharacter with: 65 with: 65.0 with: true.
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 11/14/2023 10:37:38'!
testGenericInt32Call
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestInt32: $A asCharacter with: 65 with: 65.0 with: true.
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 11/14/2023 10:38:10'!
testGenericMixedDoublesInt32AndStruct
	"Test using generic FFI spec"
	| result int struct |
	int := 42.
	struct := FFITestPoint4 new.
	struct x: 1. struct y: 2. struct z: 3. struct w: 4.
	result := FFITestLibrary ffiTestMixedDoublesInt32AndStruct: 1.0 d: 2.0 d: 3.0 d: 4.0 d: 5.0 d: 6.0 d: 7.0 d: 8.0 d: 9.0 i: int s: struct.
	self assert: result = (45.0 + 42 + 10)! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 12/17/2018 12:44:56'!
testGenericMixedFloatsAndDouble
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestMixedFloatsAndDouble: 1.2 with: 3.4 with: 5.6 with: 7.8.
	self assert: (1.2 + 3.4 + 5.6 + 7.8 - result) abs < 1.0e-6! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 1/6/2023 10:24:01'!
testGenericPrintString
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiPrintString:'Hello World!!' asUtf8BytesOrByteString.
	self assert: result = 'Hello World!!'.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 11/14/2023 09:54:51'!
testInt64A1
	"Test passing a char and two int64s."
	| byte long1 long2 long3 |
	byte := 42.
	long1 := 16r123456789012.
	long2 := (-1 << 31).
	long3 := FFITestLibrary ffiTestInt64A1: byte with: long1 with: long2.
	self assert: long3 = (byte + long1 + long2)! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 11/14/2023 09:54:55'!
testInt64A3
	"Test passing a char, a int64, and another char."
	| byte1 long1 byte2 long2 |
	byte1 := 3.
	long1 := 16r123456789012.
	byte2 := 4.
	long2 := FFITestLibrary ffiTestInt64A3: byte1 with: long1 with: byte2.
	self assert: long2 = (byte1 + long1 + byte2)! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 1/6/2023 10:36:42'!
testLibraryCharCall
	"Test using call from ExternalLibrary"
	| result |
	result := FFITestLibrary new ffiTestChar: $A asCharacter with: 65 with: 65.0 with: true.
	self assert: result class = Character.
	self assert: result numericValue = 130! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 1/6/2023 10:55:43'!
testLibraryDoubleCall
	"Test using call from ExternalLibrary"
	| result |
	result := FFITestLibrary new ffiTestDoubles: $A asCharacter with: 65.0.
	self assert: result = 130.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 1/6/2023 10:55:53'!
testLibraryFloatCall
	"Test using call from ExternalLibrary"
	| result |
	result := FFITestLibrary new ffiTestFloats: $A asCharacter with: 65.0.
	self assert: result = 130.0! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 11/14/2023 10:35:10'!
testLibraryInt32Call
	"Test using call from ExternalLibrary"
	| result |
	result := FFITestLibrary new ffiTestInt32: $A asCharacter with: 65 with: 65.0 with: true.
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 1/6/2023 10:24:06'!
testLibraryPrintString
	"Test using call from ExternalLibrary"
	| result |
	result := FFITestLibrary new ffiPrintString:'Hello World!!' asUtf8BytesOrByteString.
	self assert: result = 'Hello World!!'.! !

!FFIPluginTests methodsFor: 'simple tests' stamp: 'jmv 11/14/2023 10:05:18'!
testUint32Range
	"Simple test for making sure the FFI can call certain numbers in the uint32 range"
	| result |
	self shouldnt:[result := FFITestLibrary ffiTestUInt32: 3103854339 with: 3103854339 with: 3103854339 with: 3103854339] raise: Error.
	self should: result =  1912741382.! !

!FFIPluginTests methodsFor: 'other tests' stamp: 'jmv 1/6/2023 10:59:00'!
testLoadSymbolFromModule
	| address |
	
	"Ok"
	address := ExternalAddress loadSymbol: 'ffiTestFloats' asUtf8BytesOrByteString module: FFITestLibrary moduleName asUtf8BytesOrByteString.
	self deny: address isNil.
	self deny: address isNull.
	"Symbol does not exists"
	self 
		should: [ ExternalAddress loadSymbol: 'ffi_does_not_exist' asUtf8BytesOrByteString module: FFITestLibrary moduleName asUtf8BytesOrByteString ]
		raise: Error.
	"Module does not exists"
	self 
		should: [ ExternalAddress loadSymbol: 'ffiTestFloats' asUtf8BytesOrByteString module: 'NonExistingModule' asUtf8BytesOrByteString ]
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

!FFIPluginTests methodsFor: 'structure tests' stamp: 'jmv 11/14/2023 09:54:59'!
testInt64s
	"Test passing and returning int64s"
	| long1 long2 long3 |
	long1 := 16r123456789012.
	long2 := (-1 << 31).
	long3 := FFITestLibrary ffiTestInt64: long1 with: long2.
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

!FFIPluginTests methodsFor: 'old typeNames tests' stamp: 'jmv 1/6/2023 10:55:32'!
testGenericIntCall
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestInt: $A asCharacter with: 65 with: 65.0 with: true.
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'old typeNames tests' stamp: 'jmv 2/20/2019 00:15:10'!
testGenericMixedDoublesIntAndStruct
	"Test using generic FFI spec"
	| result int struct |
	int := 42.
	struct := FFITestPoint4 new.
	struct x: 1. struct y: 2. struct z: 3. struct w: 4.
	result := FFITestLibrary ffiTestMixedDoublesIntAndStruct: 1.0 d: 2.0 d: 3.0 d: 4.0 d: 5.0 d: 6.0 d: 7.0 d: 8.0 d: 9.0 i: int s: struct.
	self assert: result = (45.0 + 42 + 10)! !

!FFIPluginTests methodsFor: 'old typeNames tests' stamp: 'jmv 1/6/2023 10:53:52'!
testGenericShortCall
	"Test using generic FFI spec"
	| result |
	result := FFITestLibrary ffiTestShort: $A asCharacter with: 65 with: 65.0 with: true.
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'old typeNames tests' stamp: 'jmv 1/6/2023 10:56:03'!
testLibraryIntCall
	"Test using call from ExternalLibrary"
	| result |
	result := FFITestLibrary new ffiTestInt: $A asCharacter with: 65 with: 65.0 with: true.
	self assert: result = 130.! !

!FFIPluginTests methodsFor: 'old typeNames tests' stamp: 'djm 5/6/2014 01:54'!
testLongLongA1
	"Test passing a char and two longlongs."
	| byte long1 long2 long3 |
	byte := 42.
	long1 := 16r123456789012.
	long2 := (-1 << 31).
	long3 := FFITestLibrary ffiTestLongLongA1: byte with: long1 with: long2.
	self assert: long3 = (byte + long1 + long2)! !

!FFIPluginTests methodsFor: 'old typeNames tests' stamp: 'djm 5/6/2014 02:50'!
testLongLongA3
	"Test passing a char, a longlong, and another char."
	| byte1 long1 byte2 long2 |
	byte1 := 3.
	long1 := 16r123456789012.
	byte2 := 4.
	long2 := FFITestLibrary ffiTestLongLongA3: byte1 with: long1 with: byte2.
	self assert: long2 = (byte1 + long1 + byte2)! !

!FFIPluginTests methodsFor: 'old typeNames tests' stamp: 'ar 8/14/2006 19:23'!
testLongLongs
	"Test passing and returning longlongs"
	| long1 long2 long3 |
	long1 := 16r123456789012.
	long2 := (-1 << 31).
	long3 := FFITestLibrary ffiTestLongLong: long1 with: long2.
	self assert: long3 = (long1 + long2)! !

!FFIPluginTests methodsFor: 'old typeNames tests' stamp: 'ar 7/28/2009 20:53'!
testUlongRange
	"Simple test for making sure the FFI can call certain numbers in the ulong range"
	| result |
	self shouldnt:[result := FFITestLibrary ffiTestUlong: 3103854339 with: 3103854339 with: 3103854339 with: 3103854339] raise: Error.
	self should: result =  1912741382.! !

!FFITestLibrary methodsFor: 'primitives' stamp: 'jmv 1/6/2023 10:52:11'!
ffiPrintString: aString
	"FFITestLibrary new ffiPrintString: 'Hello' asUtf8BytesOrByteString"
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

!FFITestLibrary methodsFor: 'primitives' stamp: 'jmv 11/14/2023 10:36:21'!
ffiTestInt16: c1 with: c2 with: c3 with: c4
	"FFITestLibrary new ffiTestInt16: $A with: 65 with: 65.0 with: $A"
	<cdecl: int16 'ffiTestShorts' (int16 int16 int16 int16)>
	^self externalCallFailed! !

!FFITestLibrary methodsFor: 'primitives' stamp: 'jmv 11/14/2023 10:16:11'!
ffiTestInt32: c1 with: c2 with: c3 with: c4
	"FFITestLibrary new ffiTestInt: $A with: 65 with: 65.0 with: $A"
	<cdecl: int32 'ffiTestInts' (int32 int32 int32 int32)>
	^self externalCallFailed! !

!FFITestLibrary methodsFor: 'old typeNames' stamp: 'jmv 11/14/2023 11:33:41'!
ffiTestInt: c1 with: c2 with: c3 with: c4
	"FFITestLibrary new ffiTestInt: $A with: 65 with: 65.0 with: $A"
	<cdecl: long 'ffiTestInts' (long long long long)>
	^self externalCallFailed! !

!FFITestLibrary methodsFor: 'old typeNames' stamp: 'jmv 11/14/2023 11:33:30'!
ffiTestShort: c1 with: c2 with: c3 with: c4
	"FFITestLibrary new ffiTestShort: $A with: 65 with: 65.0 with: $A"
	<cdecl: short 'ffiTestShorts' (short short short short)>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'accessing' stamp: 'ar 11/28/1999 19:51'!
moduleName
	"Use the fully qualified VM name so we ensure testing loading a library"
	^'SqueakFFIPrims'! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'jmv 1/6/2023 10:52:16'!
ffiPrintString: aString
	"FFITestLibrary ffiPrintString: 'Hello' asUtf8BytesOrByteString"
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

!FFITestLibrary class methodsFor: 'primitives' stamp: 'jmv 11/14/2023 10:36:01'!
ffiTestInt16: c1 with: c2 with: c3 with: c4
	"FFITestLibrary ffiTestInt16: $A with: 65 with: 65.0 with:1"
	<cdecl: int16 'ffiTestShorts' (int16 int16 int16 int16) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'jmv 11/14/2023 10:15:28'!
ffiTestInt32: c1 with: c2 with: c3 with: c4
	"FFITestLibrary ffiTestInt32: $A with: 65 with: 65.0 with: true"
	<cdecl: int32 'ffiTestInts' (int32 int32 int32 int32) module: 'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'jmv 11/14/2023 09:55:15'!
ffiTestInt64: long1 with: long2
	"FFITestLibrary ffiTestInt64: 3 with: 4"
	<cdecl: int64 'ffiTestLongLong' (int64 int64) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'jmv 11/14/2023 09:55:24'!
ffiTestInt64A1: byte with: long1 with: long2
	"FFITestLibrary ffiTestInt64A1: 3 with: 4 with: 5"
	<cdecl: int64 'ffiTestLongLonga1' (char int64 int64) module: 'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'jmv 11/14/2023 09:55:34'!
ffiTestInt64A3: byte1 with: long1 with: byte2
	"FFITestLibrary ffiTestInt64A3: 3 with: 4 with: 5"
	<cdecl: int64 'ffiTestLongLonga3' (char int64 char) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'jmv 11/14/2023 10:38:01'!
ffiTestMixedDoublesInt32AndStruct: f1 d: f2 d: f3 d: f4 d: f5 d: f6 d: f7 d: f8 d: f9 i: i1 s: s1
	"FFITestLibrary ffiTestMixedDoublesIntAndStruct: 1.0 d: 2.0 d: 3.0 d: 4.0 d: 5.0 d: 6.0 d: 7.0 d: 8.0 d: 9.0 i: 42
		s: (FFITestPoint4 new x: 3; y: 4; z: 5; w:6)"
	<cdecl: double 'ffiTestMixedDoublesIntAndStruct' (double double double double double double double double double int32 FFITestPoint4) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'djm 5/6/2014 05:08'!
ffiTestMixedFloatsAndDouble: f1 with: d1 with: f2 with: f3
	"FFITestLibrary ffiTestMixedFloatsAndDouble: 1.2 with: 3.4 with: 5.6 with: 7.8"
	<cdecl: double 'ffiTestMixedFloatsAndDouble' (float double float float) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'jmv 11/14/2023 10:17:56'!
ffiTestMixedIntAndStruct2: i with: pt4
	"FFITestLibrary ffiTestMixedIntAndStruct2: 2 with: (FFITestPoint4 new x: 3; y: 4; z: 5; w:6)"
	<cdecl: int32 'ffiTestMixedIntAndStruct2' (int32 FFITestPoint4) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'jmv 11/14/2023 10:18:14'!
ffiTestMixedIntAndStruct3: i with: anFFISmallStruct1
	"FFITestLibrary ffiTestMixedIntAndStruct3: 2 with: (FFISmallStruct1 new x: 3; y: 4)"
	<cdecl: int32 'ffiTestMixedIntAndStruct3' (int32 FFISmallStruct1) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'jmv 11/14/2023 10:18:26'!
ffiTestMixedIntAndStruct: i with: pt1 with: pt2
	"FFITestLibrary ffiTestMixedIntAndStruct: 2 with: (FFITestPoint2 new x: 3; y: 4) with: (FFITestPoint2 new x: 5; y: 6)"
	<cdecl: int32 'ffiTestMixedIntAndStruct' (int32 FFITestPoint2 FFITestPoint2) module:'SqueakFFIPrims'>
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

!FFITestLibrary class methodsFor: 'primitives' stamp: 'djm 5/6/2014 05:30'!
ffiTestSmallStructReturn
	"FFITestLibrary ffiTestSmallStructReturn"
	<cdecl: FFISmallStruct1 'ffiTestSmallStructReturn' (void) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'primitives' stamp: 'jmv 11/14/2023 10:08:26'!
ffiTestUInt32: c1 with: c2 with: c3 with: c4
	"FFITestLibrary ffiTestUInt32: 3103854339 with: 3103854339 with: 3103854339 with: 3103854339"
	<cdecl: int32 'ffiTestInts' (uint32 uint32 uint32 uint32) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'old typeNames' stamp: 'jmv 11/14/2023 11:33:43'!
ffiTestInt: c1 with: c2 with: c3 with: c4
	"FFITestLibrary ffiTestInt: $A with: 65 with: 65.0 with: true"
	<cdecl: long 'ffiTestInts' (long long long long) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'old typeNames' stamp: 'jmv 11/14/2023 11:32:57'!
ffiTestLongLong: long1 with: long2
	"FFITestLibrary ffiTestLongLong: 3 with: 4"
	<cdecl: longlong 'ffiTestLongLong' (longlong longlong) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'old typeNames' stamp: 'jmv 11/14/2023 11:30:44'!
ffiTestLongLongA1: byte with: long1 with: long2
	"FFITestLibrary ffiTestLongLongA1: 3 with: 4 with: 5"
	<cdecl: longlong 'ffiTestLongLonga1' (char longlong longlong) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'old typeNames' stamp: 'jmv 11/14/2023 11:33:50'!
ffiTestLongLongA3: byte1 with: long1 with: byte2
	"FFITestLibrary ffiTestLongLongA3: 3 with: 4 with: 5"
	<cdecl: longlong 'ffiTestLongLonga3' (char longlong char) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'old typeNames' stamp: 'jmv 11/14/2023 11:33:20'!
ffiTestMixedDoublesIntAndStruct: f1 d: f2 d: f3 d: f4 d: f5 d: f6 d: f7 d: f8 d: f9 i: i1 s: s1
	"FFITestLibrary ffiTestMixedDoublesIntAndStruct: 1.0 d: 2.0 d: 3.0 d: 4.0 d: 5.0 d: 6.0 d: 7.0 d: 8.0 d: 9.0 i: 42
		s: (FFITestPoint4 new x: 3; y: 4; z: 5; w:6)"
	<cdecl: double 'ffiTestMixedDoublesIntAndStruct' (double double double double double double double double double long FFITestPoint4) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'old typeNames' stamp: 'jmv 11/14/2023 11:33:33'!
ffiTestShort: c1 with: c2 with: c3 with: c4
	"FFITestLibrary ffiTestShort: $A with: 65 with: 65.0 with:1"
	<cdecl: short 'ffiTestShorts' (short short short short) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFITestLibrary class methodsFor: 'old typeNames' stamp: 'jmv 11/14/2023 11:34:04'!
ffiTestUlong: c1 with: c2 with: c3 with: c4
	"FFITestLibrary ffiTestUlong: 3103854339 with: 3103854339 with: 3103854339 with: 3103854339"
	<cdecl: long 'ffiTestInts' (ulong ulong ulong ulong) module:'SqueakFFIPrims'>
	^self externalCallFailed! !

!FFISmallStruct1 methodsFor: 'accessing'!
x
	"This method was automatically generated. See FFISmallStruct1 class>>fields."
	<generated>
	^handle ffiUint8At: 1! !

!FFISmallStruct1 methodsFor: 'accessing'!
x: anObject
	"This method was automatically generated. See FFISmallStruct1 class>>fields."
	<generated>
	handle ffiUint8At: 1 put: anObject! !

!FFISmallStruct1 methodsFor: 'accessing'!
y
	"This method was automatically generated. See FFISmallStruct1 class>>fields."
	<generated>
	^handle ffiUint8At: 2! !

!FFISmallStruct1 methodsFor: 'accessing'!
y: anObject
	"This method was automatically generated. See FFISmallStruct1 class>>fields."
	<generated>
	handle ffiUint8At: 2 put: anObject! !

!FFISmallStruct1 class methodsFor: 'as yet unclassified' stamp: 'jmv 11/14/2023 10:48:25'!
fields
	"FFISmallStruct1 defineFields"
	^#(
		(x	'uint8')
		(y	'uint8')
	)! !

!FFISmallStruct1 class methodsFor: 'class initialization' stamp: 'jmv 12/19/2016 15:55:57'!
initialize
	self compileFields! !

!FFITestBiggerStruct methodsFor: 'accessing'!
r
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	^handle ffiInt64At: 33! !

!FFITestBiggerStruct methodsFor: 'accessing'!
r: anObject
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	handle ffiInt64At: 33 put: anObject! !

!FFITestBiggerStruct methodsFor: 'accessing'!
s
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	^handle ffiInt64At: 41! !

!FFITestBiggerStruct methodsFor: 'accessing'!
s: anObject
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	handle ffiInt64At: 41 put: anObject! !

!FFITestBiggerStruct methodsFor: 'accessing'!
t
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	^handle ffiInt64At: 49! !

!FFITestBiggerStruct methodsFor: 'accessing'!
t: anObject
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	handle ffiInt64At: 49 put: anObject! !

!FFITestBiggerStruct methodsFor: 'accessing'!
u
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	^handle ffiInt64At: 57! !

!FFITestBiggerStruct methodsFor: 'accessing'!
u: anObject
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	handle ffiInt64At: 57 put: anObject! !

!FFITestBiggerStruct methodsFor: 'accessing'!
w
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	^handle ffiInt64At: 25! !

!FFITestBiggerStruct methodsFor: 'accessing'!
w: anObject
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	handle ffiInt64At: 25 put: anObject! !

!FFITestBiggerStruct methodsFor: 'accessing'!
x
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	^handle ffiInt64At: 1! !

!FFITestBiggerStruct methodsFor: 'accessing'!
x: anObject
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	handle ffiInt64At: 1 put: anObject! !

!FFITestBiggerStruct methodsFor: 'accessing'!
y
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	^handle ffiInt64At: 9! !

!FFITestBiggerStruct methodsFor: 'accessing'!
y: anObject
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	handle ffiInt64At: 9 put: anObject! !

!FFITestBiggerStruct methodsFor: 'accessing'!
z
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	^handle ffiInt64At: 17! !

!FFITestBiggerStruct methodsFor: 'accessing'!
z: anObject
	"This method was automatically generated. See FFITestBiggerStruct class>>fields."
	<generated>
	handle ffiInt64At: 17 put: anObject! !

!FFITestBiggerStruct class methodsFor: 'field definition' stamp: 'jmv 11/14/2023 12:13:58'!
fields
	"FFITestBiggerStruct defineFields"
	^#(
		(x	'int64')
		(y	'int64')
		(z	'int64')
		(w	'int64')
		(r  	'int64')
		(s  	'int64')
		(t  	'int64')
		(u  	'int64')
	)! !

!FFITestBiggerStruct class methodsFor: 'class initialization' stamp: 'KenD 2/7/2019 03:04:32'!
initialize
"
	self defineFields; compileFields.
"
	self compileFields! !

!FFITestPoint2 methodsFor: 'accessing'!
x
	"This method was automatically generated. See FFITestPoint2 class>>fields."
	<generated>
	^handle ffiInt32At: 1! !

!FFITestPoint2 methodsFor: 'accessing'!
x: anObject
	"This method was automatically generated. See FFITestPoint2 class>>fields."
	<generated>
	handle ffiInt32At: 1 put: anObject! !

!FFITestPoint2 methodsFor: 'accessing'!
y
	"This method was automatically generated. See FFITestPoint2 class>>fields."
	<generated>
	^handle ffiInt32At: 5! !

!FFITestPoint2 methodsFor: 'accessing'!
y: anObject
	"This method was automatically generated. See FFITestPoint2 class>>fields."
	<generated>
	handle ffiInt32At: 5 put: anObject! !

!FFITestPoint2 class methodsFor: 'field definition' stamp: 'jmv 11/14/2023 10:09:36'!
fields
	"FFITestPoint2 defineFields"
	^#(
		(x	'int32')
		(y	'int32')
	)! !

!FFITestPoint2 class methodsFor: 'class initialization' stamp: 'jmv 8/21/2013 17:39'!
initialize
	self compileFields! !

!FFITestPoint4 methodsFor: 'accessing'!
w
	"This method was automatically generated. See FFITestPoint4 class>>fields."
	<generated>
	^handle ffiInt32At: 13! !

!FFITestPoint4 methodsFor: 'accessing'!
w: anObject
	"This method was automatically generated. See FFITestPoint4 class>>fields."
	<generated>
	handle ffiInt32At: 13 put: anObject! !

!FFITestPoint4 methodsFor: 'accessing'!
x
	"This method was automatically generated. See FFITestPoint4 class>>fields."
	<generated>
	^handle ffiInt32At: 1! !

!FFITestPoint4 methodsFor: 'accessing'!
x: anObject
	"This method was automatically generated. See FFITestPoint4 class>>fields."
	<generated>
	handle ffiInt32At: 1 put: anObject! !

!FFITestPoint4 methodsFor: 'accessing'!
y
	"This method was automatically generated. See FFITestPoint4 class>>fields."
	<generated>
	^handle ffiInt32At: 5! !

!FFITestPoint4 methodsFor: 'accessing'!
y: anObject
	"This method was automatically generated. See FFITestPoint4 class>>fields."
	<generated>
	handle ffiInt32At: 5 put: anObject! !

!FFITestPoint4 methodsFor: 'accessing'!
z
	"This method was automatically generated. See FFITestPoint4 class>>fields."
	<generated>
	^handle ffiInt32At: 9! !

!FFITestPoint4 methodsFor: 'accessing'!
z: anObject
	"This method was automatically generated. See FFITestPoint4 class>>fields."
	<generated>
	handle ffiInt32At: 9 put: anObject! !

!FFITestPoint4 class methodsFor: 'field definition' stamp: 'jmv 11/14/2023 10:10:10'!
fields
	"FFITestPoint4 defineFields"
	^#(
		(x	'int32')
		(y	'int32')
		(z	'int32')
		(w	'int32')
	)! !

!FFITestPoint4 class methodsFor: 'class initialization' stamp: 'jmv 8/21/2013 17:39'!
initialize
	self compileFields! !
FFISmallStruct1 initialize!
FFITestBiggerStruct initialize!
FFITestPoint2 initialize!
FFITestPoint4 initialize!
