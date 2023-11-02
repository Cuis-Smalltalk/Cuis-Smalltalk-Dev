'From Cuis 6.0 [latest update: #6055] on 25 October 2023 at 10:50:00 am'!
'Description '!
!provides: 'TestDynamicCuisLibraries' 1 15!
!requires: 'DynamicCuisLibraries' 1 22 nil!
SystemOrganization addCategory: #TestDynamicCuisLibraries!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-Tests'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClassesBasic'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClasses01'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClasses02'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClasses03'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClasses04'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClasses05'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-SubjectClasses06'!
SystemOrganization addCategory: #'TestDynamicCuisLibraries-Transient'!


!classDefinition: #Noclash_Float32ArrayCodeModelExperiment category: #'TestDynamicCuisLibraries-SubjectClassesBasic'!
FloatArray variableWordSubclass: #Noclash_Float32ArrayCodeModelExperiment
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClassesBasic'!
!classDefinition: 'Noclash_Float32ArrayCodeModelExperiment class' category: #'TestDynamicCuisLibraries-SubjectClassesBasic'!
Noclash_Float32ArrayCodeModelExperiment class
	instanceVariableNames: ''!

!classDefinition: #CodeModelTest1 category: #'TestDynamicCuisLibraries-Tests'!
TestCase subclass: #CodeModelTest1
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-Tests'!
!classDefinition: 'CodeModelTest1 class' category: #'TestDynamicCuisLibraries-Tests'!
CodeModelTest1 class
	instanceVariableNames: ''!

!classDefinition: #DclBasicTest category: #'TestDynamicCuisLibraries-Tests'!
TestCase subclass: #DclBasicTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-Tests'!
!classDefinition: 'DclBasicTest class' category: #'TestDynamicCuisLibraries-Tests'!
DclBasicTest class
	instanceVariableNames: ''!

!classDefinition: #DclTest category: #'TestDynamicCuisLibraries-Tests'!
TestCase subclass: #DclTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-Tests'!
!classDefinition: 'DclTest class' category: #'TestDynamicCuisLibraries-Tests'!
DclTest class
	instanceVariableNames: ''!

!classDefinition: #Noclash_CodeModelExperiment category: #'TestDynamicCuisLibraries-SubjectClassesBasic'!
Object subclass: #Noclash_CodeModelExperiment
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClassesBasic'!
!classDefinition: 'Noclash_CodeModelExperiment class' category: #'TestDynamicCuisLibraries-SubjectClassesBasic'!
Noclash_CodeModelExperiment class
	instanceVariableNames: ''!

!classDefinition: #Noclash_PointCodeModelExperiment category: #'TestDynamicCuisLibraries-SubjectClassesBasic'!
Object subclass: #Noclash_PointCodeModelExperiment
	instanceVariableNames: 'x y'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClassesBasic'!
!classDefinition: 'Noclash_PointCodeModelExperiment class' category: #'TestDynamicCuisLibraries-SubjectClassesBasic'!
Noclash_PointCodeModelExperiment class
	instanceVariableNames: ''!

!classDefinition: #Noclash_DclExample01 category: #'TestDynamicCuisLibraries-SubjectClasses01'!
Object subclass: #Noclash_DclExample01
	instanceVariableNames: 'a b c'
	classVariableNames: 'A B'
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses01'!
!classDefinition: 'Noclash_DclExample01 class' category: #'TestDynamicCuisLibraries-SubjectClasses01'!
Noclash_DclExample01 class
	instanceVariableNames: 'aa bb'!

!classDefinition: #NoclashExisting_DclExample02 category: #'TestDynamicCuisLibraries-SubjectClasses02'!
Object subclass: #NoclashExisting_DclExample02
	instanceVariableNames: 'a b c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses02'!
!classDefinition: 'NoclashExisting_DclExample02 class' category: #'TestDynamicCuisLibraries-SubjectClasses02'!
NoclashExisting_DclExample02 class
	instanceVariableNames: 'aa bb'!

!classDefinition: #Noclash_DclExample02 category: #'TestDynamicCuisLibraries-SubjectClasses02'!
Object subclass: #Noclash_DclExample02
	instanceVariableNames: 'a b c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses02'!
!classDefinition: 'Noclash_DclExample02 class' category: #'TestDynamicCuisLibraries-SubjectClasses02'!
Noclash_DclExample02 class
	instanceVariableNames: 'aa bb'!

!classDefinition: #NoclashABCnoClassIvars_DclExample03 category: #'TestDynamicCuisLibraries-SubjectClasses03'!
Object subclass: #NoclashABCnoClassIvars_DclExample03
	instanceVariableNames: 'a b c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses03'!
!classDefinition: 'NoclashABCnoClassIvars_DclExample03 class' category: #'TestDynamicCuisLibraries-SubjectClasses03'!
NoclashABCnoClassIvars_DclExample03 class
	instanceVariableNames: 'zz xx'!

!classDefinition: #NoclashABC_DclExample03 category: #'TestDynamicCuisLibraries-SubjectClasses03'!
Object subclass: #NoclashABC_DclExample03
	instanceVariableNames: 'a b c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses03'!
!classDefinition: 'NoclashABC_DclExample03 class' category: #'TestDynamicCuisLibraries-SubjectClasses03'!
NoclashABC_DclExample03 class
	instanceVariableNames: ''!

!classDefinition: #NoclashABCD_DclExample03Subclass category: #'TestDynamicCuisLibraries-SubjectClasses03'!
NoclashABC_DclExample03 subclass: #NoclashABCD_DclExample03Subclass
	instanceVariableNames: 'd'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses03'!
!classDefinition: 'NoclashABCD_DclExample03Subclass class' category: #'TestDynamicCuisLibraries-SubjectClasses03'!
NoclashABCD_DclExample03Subclass class
	instanceVariableNames: ''!

!classDefinition: #NoclashAB_DclExample03 category: #'TestDynamicCuisLibraries-SubjectClasses03'!
Object subclass: #NoclashAB_DclExample03
	instanceVariableNames: 'a b'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses03'!
!classDefinition: 'NoclashAB_DclExample03 class' category: #'TestDynamicCuisLibraries-SubjectClasses03'!
NoclashAB_DclExample03 class
	instanceVariableNames: ''!

!classDefinition: #NoclashABD_DclExample03Subclass category: #'TestDynamicCuisLibraries-SubjectClasses03'!
NoclashAB_DclExample03 subclass: #NoclashABD_DclExample03Subclass
	instanceVariableNames: 'd'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses03'!
!classDefinition: 'NoclashABD_DclExample03Subclass class' category: #'TestDynamicCuisLibraries-SubjectClasses03'!
NoclashABD_DclExample03Subclass class
	instanceVariableNames: ''!

!classDefinition: #NoclashACB_DclExample03 category: #'TestDynamicCuisLibraries-SubjectClasses03'!
Object subclass: #NoclashACB_DclExample03
	instanceVariableNames: 'a c b'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses03'!
!classDefinition: 'NoclashACB_DclExample03 class' category: #'TestDynamicCuisLibraries-SubjectClasses03'!
NoclashACB_DclExample03 class
	instanceVariableNames: ''!

!classDefinition: #NoclashABC_DclExample04 category: #'TestDynamicCuisLibraries-SubjectClasses04'!
Object subclass: #NoclashABC_DclExample04
	instanceVariableNames: 'a b c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses04'!
!classDefinition: 'NoclashABC_DclExample04 class' category: #'TestDynamicCuisLibraries-SubjectClasses04'!
NoclashABC_DclExample04 class
	instanceVariableNames: ''!

!classDefinition: #NoclashAC_DclExample04 category: #'TestDynamicCuisLibraries-SubjectClasses04'!
Object subclass: #NoclashAC_DclExample04
	instanceVariableNames: 'a c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses04'!
!classDefinition: 'NoclashAC_DclExample04 class' category: #'TestDynamicCuisLibraries-SubjectClasses04'!
NoclashAC_DclExample04 class
	instanceVariableNames: ''!

!classDefinition: #NoclashB_DclExample04Sub category: #'TestDynamicCuisLibraries-SubjectClasses04'!
NoclashAC_DclExample04 subclass: #NoclashB_DclExample04Sub
	instanceVariableNames: 'b'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses04'!
!classDefinition: 'NoclashB_DclExample04Sub class' category: #'TestDynamicCuisLibraries-SubjectClasses04'!
NoclashB_DclExample04Sub class
	instanceVariableNames: ''!

!classDefinition: #NoclashByte_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Object variableByteSubclass: #NoclashByte_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashByte_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashByte_DclExample05 class
	instanceVariableNames: ''!

!classDefinition: #NoclashDoubleByte_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Object variableDoubleByteSubclass: #NoclashDoubleByte_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashDoubleByte_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashDoubleByte_DclExample05 class
	instanceVariableNames: ''!

!classDefinition: #NoclashDoubleWord_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Object variableDoubleWordSubclass: #NoclashDoubleWord_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashDoubleWord_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashDoubleWord_DclExample05 class
	instanceVariableNames: ''!

!classDefinition: #NoclashIndexed_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Object variableSubclass: #NoclashIndexed_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashIndexed_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashIndexed_DclExample05 class
	instanceVariableNames: ''!

!classDefinition: #NoclashObject_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Object subclass: #NoclashObject_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashObject_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashObject_DclExample05 class
	instanceVariableNames: ''!

!classDefinition: #NoclashWeak_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Object weakSubclass: #NoclashWeak_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashWeak_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashWeak_DclExample05 class
	instanceVariableNames: ''!

!classDefinition: #NoclashWord_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Object variableWordSubclass: #NoclashWord_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashWord_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashWord_DclExample05 class
	instanceVariableNames: ''!

!classDefinition: #Noclash_DclExample05Super category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Object subclass: #Noclash_DclExample05Super
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'Noclash_DclExample05Super class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Noclash_DclExample05Super class
	instanceVariableNames: ''!

!classDefinition: #NoclashSub_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
Noclash_DclExample05Super subclass: #NoclashSub_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashSub_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashSub_DclExample05 class
	instanceVariableNames: ''!

!classDefinition: #NoclashABCclasVarZZ_DclExample06 category: #'TestDynamicCuisLibraries-SubjectClasses06'!
Object subclass: #NoclashABCclasVarZZ_DclExample06
	instanceVariableNames: 'a b c'
	classVariableNames: 'ZZ'
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses06'!
!classDefinition: 'NoclashABCclasVarZZ_DclExample06 class' category: #'TestDynamicCuisLibraries-SubjectClasses06'!
NoclashABCclasVarZZ_DclExample06 class
	instanceVariableNames: ''!

!classDefinition: #NoclashABC_DclExample06 category: #'TestDynamicCuisLibraries-SubjectClasses06'!
Object subclass: #NoclashABC_DclExample06
	instanceVariableNames: 'a b c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses06'!
!classDefinition: 'NoclashABC_DclExample06 class' category: #'TestDynamicCuisLibraries-SubjectClasses06'!
NoclashABC_DclExample06 class
	instanceVariableNames: ''!

!classDefinition: #NoclashProtoObject_DclExample05 category: #'TestDynamicCuisLibraries-SubjectClasses05'!
ProtoObject subclass: #NoclashProtoObject_DclExample05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TestDynamicCuisLibraries-SubjectClasses05'!
!classDefinition: 'NoclashProtoObject_DclExample05 class' category: #'TestDynamicCuisLibraries-SubjectClasses05'!
NoclashProtoObject_DclExample05 class
	instanceVariableNames: ''!


!Noclash_Float32ArrayCodeModelExperiment methodsFor: 'accessing' stamp: 'jmv 8/2/2023 16:44:31'!
floatAt: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>! !

!Noclash_Float32ArrayCodeModelExperiment methodsFor: 'accessing' stamp: 'jmv 8/2/2023 16:44:31'!
floatAt: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>! !

!CodeModelTest1 methodsFor: 'testing' stamp: 'jmv 8/2/2023 17:21:42'!
testAllInstanceSideMethods
	| classSpecs cmSpecs methods newMethods |
	classSpecs := OrderedDictionary new.
	Object withAllSubclassesDo: [ :cls |
		cls isMeta ifFalse: [
			methods := cls methodDict asArray.
			cmSpecs := OrderedCollection new.
			methods do: [ :cm |
				cmSpecs add: (MethodSpec for: cm addClassesTo: classSpecs) ].
			newMethods := cmSpecs collect: [ :methodSpec | methodSpec materializeIn: cls ].
			newMethods with: methods do: [ :newCm :cm |
				self assert: newCm = cm ]]]! !

!DclBasicTest methodsFor: 'testing' stamp: 'jmv 8/4/2023 13:05:24'!
testCodeModelExperiment
	| lib oldClass newClass |
	Smalltalk at: #CodeModelExperiment ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	oldClass := Noclash_CodeModelExperiment.
	lib := DynamicCuisLibrary
		withClasses: { oldClass name }
		classSelectors: #( #() )
		instanceSelectors: #( #( method01) ).
	self assert: lib canBeInstalled.
	lib installLibrary.
	newClass := Smalltalk at: #CodeModelExperiment.
	self assert: (oldClass compiledMethodAt: #method01) =
		(newClass compiledMethodAt: #method01).
	self assert: Noclash_CodeModelExperiment new method01 = newClass new method01.
	newClass removeFromSystemUnlogged.! !

!DclBasicTest methodsFor: 'testing' stamp: 'jmv 8/4/2023 13:05:33'!
testFloatArray
	| lib oldClass newClass |
	Smalltalk at: #Float32ArrayCodeModelExperiment ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	oldClass := Noclash_Float32ArrayCodeModelExperiment.
	lib := DynamicCuisLibrary
		withClasses: { oldClass name }
		classSelectors: #( #() )
		instanceSelectors: #( #(floatAt: floatAt:put:) ).	
	self assert: lib canBeInstalled.
	lib installLibrary.
	newClass := Smalltalk at: #Float32ArrayCodeModelExperiment.
	self assert: (oldClass compiledMethodAt: #floatAt:) =
		(newClass compiledMethodAt: #floatAt:).
	self assert: ((newClass with: 1.23) + (newClass with: 2.77)) first = 4.0.
	self assert: (newClass with: 1.23 with: 2.77) sum = 4.0.
	newClass removeFromSystemUnlogged.! !

!DclBasicTest methodsFor: 'testing' stamp: 'jmv 8/4/2023 13:05:40'!
testPoint
	| lib oldClass newClass |
	Smalltalk at: #PointCodeModelExperiment ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	oldClass := Noclash_PointCodeModelExperiment.
	lib := DynamicCuisLibrary
		withClasses: { oldClass name }
		classSelectors: #( #(x:y:) )
		instanceSelectors: #( #( initializePvtX:y: x y #printOn:) ).
	self assert: lib canBeInstalled.
	lib installLibrary.
	newClass := Smalltalk at: #PointCodeModelExperiment.
	self assert: (oldClass compiledMethodAt: #initializePvtX:y:) =
		(newClass compiledMethodAt: #initializePvtX:y:).
	self assert: (newClass x: 99 y: 86) printString = '99@86'.
	self assert: (newClass x: 99 y: 86) x = 99.
	self assert: (newClass x: 99 y: 86) y = 86.
	newClass removeFromSystemUnlogged.! !

!DclTest methodsFor: 'tests 01' stamp: 'jmv 10/24/2023 10:17:34'!
test01NewClass
	"The library includes only a single class.
	Test creation of a simple class with a few instance variables and methods that access them"

	| sourceForLibrary lib installedFromLibrary |
	Smalltalk at: #DclExample01 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForLibrary := Noclash_DclExample01.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedFromLibrary := Smalltalk at: #DclExample01.
	self assert: installedFromLibrary superclass == Object.
	self assert: installedFromLibrary instVarNames equals: sourceForLibrary instVarNames.
	self assert: installedFromLibrary classVarNames equals: sourceForLibrary classVarNames.
	self assert: installedFromLibrary class instVarNames equals: sourceForLibrary class instVarNames.
	self assert: installedFromLibrary selectors equals: sourceForLibrary selectors.
	self assert: installedFromLibrary class selectors equals: sourceForLibrary class selectors.
	self assert: installedFromLibrary class >> #new equals: sourceForLibrary class >> #new.
	self assert: installedFromLibrary class >> #processor equals: sourceForLibrary class >> #processor.
	self assert: installedFromLibrary >> #initialize equals: sourceForLibrary >> #initialize.
	self assert: installedFromLibrary >> #a equals: sourceForLibrary >> #a.
	self assert: installedFromLibrary >> #b equals: sourceForLibrary >> #b.
	self assert: installedFromLibrary >> #c equals: sourceForLibrary >> #c.
	self assert: installedFromLibrary >> #combined equals: sourceForLibrary >> #combined.
	self assert: installedFromLibrary >> #maxVal equals: sourceForLibrary >> #maxVal.
	self assert: installedFromLibrary >> #someLiteral equals: sourceForLibrary >> #someLiteral.
	self assert: installedFromLibrary new combined equals: 123.
	self assert: installedFromLibrary new maxVal equals: SmallInteger maxVal.
	self assert: installedFromLibrary new someLiteral equals: sourceForLibrary new someLiteral.
	self assert: installedFromLibrary processor equals: Processor.
	installedFromLibrary setClassIVars.
	self assert: installedFromLibrary combinedCIvars equals: 57.

	installedFromLibrary removeFromSystemUnlogged.! !

!DclTest methodsFor: 'tests 02' stamp: 'jmv 10/24/2023 12:28:42'!
test02ExistingClassSameShape
	"The library includes only a single class.
	The class already exist before loading the library, and with the same shape.
	Any existing methods are exactly the same.
	Test that class shape and methods are healthy."

	| sourceForExisting existing sourceForLibrary lib installedFromLibrary |
	"Create pre-existing version of the class"
	Smalltalk at: #DclExample02 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExisting := NoclashExisting_DclExample02.
	(DynamicCuisLibrary
		withClasses: { sourceForExisting }
		classSelectors: { sourceForExisting class selectors }
		instanceSelectors: { sourceForExisting selectors }) installLibrary.

	existing := Smalltalk at: #DclExample02.
	self assert: existing new combined equals: 47.
	self assert: existing new maxVal equals: SmallInteger maxVal-1.
	self assert: existing new someLiteral equals: #(22 33).
	self assert: existing processor equals: nil.
	existing setClassIVars.
	self assert: existing combinedCIvars equals: 7.

	"Create library"
	sourceForLibrary := Noclash_DclExample02.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedFromLibrary := Smalltalk at: #DclExample02.
	self assert: installedFromLibrary instVarNames equals: sourceForLibrary instVarNames.
	self assert: installedFromLibrary selectors asSet equals: (sourceForExisting selectors, sourceForLibrary selectors) asSet.
	self assert: installedFromLibrary class selectors equals: sourceForLibrary class selectors.
	self assert: installedFromLibrary class >> #new equals: sourceForLibrary class >> #new.
	self assert: installedFromLibrary class >> #processor equals: sourceForLibrary class >> #processor.
	self assert: installedFromLibrary >> #initialize equals: sourceForLibrary >> #initialize.
	self assert: installedFromLibrary >> #a equals: sourceForLibrary >> #a.
	self assert: installedFromLibrary >> #b equals: sourceForLibrary >> #b.
	self assert: installedFromLibrary >> #c equals: sourceForLibrary >> #c.
	self assert: (installedFromLibrary >> #combined) ~= (sourceForExisting >> #combined).
	self assert: installedFromLibrary >> #combined equals: sourceForLibrary >> #combined.
	self assert: installedFromLibrary >> #maxVal equals: sourceForLibrary >> #maxVal.
	self assert: installedFromLibrary >> #someLiteral equals: sourceForLibrary >> #someLiteral.
	self assert: installedFromLibrary new combined equals: 123.
	self assert: installedFromLibrary new maxVal equals: SmallInteger maxVal.
	self assert: installedFromLibrary new someLiteral equals: sourceForLibrary new someLiteral.
	self assert: installedFromLibrary processor equals: Processor.
	self assert: installedFromLibrary class instVarNames equals: sourceForLibrary class instVarNames.
	installedFromLibrary setClassIVars.
	self assert: installedFromLibrary combinedCIvars equals: 57.

	"Cleanup"
	Smalltalk at: #DclExample02 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 03' stamp: 'jmv 10/25/2023 10:48:09'!
test03AExistingClassNewClassIVars
	"The existing class and its definition in the library differ in classInstanceVariables
	Installation should fail."

	| sourceForExisting existing sourceForLibrary lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExisting := NoclashABCnoClassIvars_DclExample03.
	(DynamicCuisLibrary
		withClasses: { sourceForExisting }
		classSelectors: { sourceForExisting class selectors }
		instanceSelectors: { sourceForExisting selectors }) installLibrary.
	existing := Smalltalk at: #DclExample03.
	self assert: existing instVarNames equals: sourceForExisting instVarNames.
	self assert: existing class allClassInstVarNames equals: sourceForExisting class allClassInstVarNames.

	"Create library"
	sourceForLibrary := NoclashABC_DclExample03.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self deny: lib canBeInstalled.

	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 03' stamp: 'jmv 8/11/2023 10:24:59'!
test03AExistingClassNewIVarsExistingInstances
	"The library adds an instance variable to a class that has existing instances.
	Installation should fail."

	| sourceForExisting existing sourceForLibrary lib anInstance |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExisting := NoclashAB_DclExample03.
	(DynamicCuisLibrary
		withClasses: { sourceForExisting }
		classSelectors: { sourceForExisting class selectors }
		instanceSelectors: { sourceForExisting selectors }) installLibrary.
	existing := Smalltalk at: #DclExample03.
	self assert: existing instVarNames equals: sourceForExisting instVarNames.
	anInstance := existing new.
	self assert: anInstance combined equals: 79.

	"Create library"
	sourceForLibrary := NoclashABC_DclExample03.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self deny: lib canBeInstalled.
	"Ensure that the instance is still around"
	self assert: anInstance notNil.

	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 03' stamp: 'jmv 8/11/2023 12:38:09'!
test03BExistingClassNewIVarsShouldMerge
	"The library adds an instance variable to a class.
	Installation should succeed.
	The new instance variable will be added after the existing ones."

	| sourceForExistingSuperclass sourceForExistingSubclass existingSuperclass existingSubclass
	 sourceForLibrary lib installedSuperclass installedSubclass |

	"Create pre-existing version of classes."
	Smalltalk at: #DclExample03Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExistingSuperclass := NoclashAB_DclExample03.
	sourceForExistingSubclass := NoclashABD_DclExample03Subclass.
	(DynamicCuisLibrary
		withClasses: { sourceForExistingSuperclass. sourceForExistingSubclass }
		classSelectors: { sourceForExistingSuperclass class selectors. sourceForExistingSubclass class selectors }
		instanceSelectors: { sourceForExistingSuperclass selectors. sourceForExistingSubclass selectors }) installLibrary.
	existingSuperclass := Smalltalk at: #DclExample03.
	existingSubclass := Smalltalk at: #DclExample03Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('a' 'b').
	self assert: existingSubclass allInstVarNames equals: #('a' 'b' 'd').
	self assert: existingSuperclass new b equals: 9.
	self assert: existingSuperclass new combined equals: 79.
	self assert: existingSubclass new b equals: 9.
	self assert: existingSubclass new d equals: 4.
	self assert: existingSubclass new combined equals: 794 + 79.

	"Create library"
	sourceForLibrary := NoclashABC_DclExample03.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedSuperclass := Smalltalk at: #DclExample03.
	installedSubclass := Smalltalk at: #DclExample03Subclass.
	self assert: installedSuperclass allInstVarNames equals: #('a' 'b' 'c').
	self assert: installedSubclass allInstVarNames equals: #('a' 'b' 'c' 'd').

	Smalltalk at: #DclExample03Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 03' stamp: 'jmv 8/11/2023 12:40:53'!
test03CExistingClassNewIVarsShouldConvertExistingMethods
	"The library adds an instance variable to a class.
	Installation should succeed.
	The new instance variable will be added after the existing ones.
	Existing methods in the subclass should be converted to access the correct instance variables.
	Existing methods in the superclass should remain unchanged, as new instance variables are added at the end."

	| sourceForExistingSuperclass sourceForExistingSubclass existingSuperclass existingSubclass
	 sourceForLibrary lib installedSuperclass installedSubclass |

	"Create pre-existing version of classes."
	Smalltalk at: #DclExample03Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExistingSuperclass := NoclashAB_DclExample03.
	sourceForExistingSubclass := NoclashABD_DclExample03Subclass.
	(DynamicCuisLibrary
		withClasses: { sourceForExistingSuperclass. sourceForExistingSubclass }
		classSelectors: { sourceForExistingSuperclass class selectors. sourceForExistingSubclass class selectors }
		instanceSelectors: { sourceForExistingSuperclass selectors. sourceForExistingSubclass selectors }) installLibrary.
	existingSuperclass := Smalltalk at: #DclExample03.
	existingSubclass := Smalltalk at: #DclExample03Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('a' 'b').
	self assert: existingSubclass allInstVarNames equals: #('a' 'b' 'd').
	self assert: existingSuperclass new b equals: 9.
	self assert: existingSuperclass new combined equals: 79.
	self assert: existingSuperclass new xk47 equals: 'xk47'.
	self assert: existingSubclass new b equals: 9.
	self assert: existingSubclass new d equals: 4.
	self assert: existingSubclass new combined equals: 794 + 79.
	self assert: existingSubclass new xk47 equals: 'xk47'.

	"Create library"
	sourceForLibrary := NoclashABC_DclExample03.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedSuperclass := Smalltalk at: #DclExample03.
	installedSubclass := Smalltalk at: #DclExample03Subclass.

	self assert: installedSuperclass allInstVarNames equals: #('a' 'b' 'c').
	self assert: installedSubclass allInstVarNames equals: #('a' 'b' 'c' 'd').

	self assert: installedSuperclass selectors asSet equals:
		(sourceForExistingSuperclass selectors, sourceForLibrary selectors) asSet.
	self assert: installedSuperclass class selectors asSet equals:
		(sourceForExistingSuperclass class selectors, sourceForLibrary class selectors) asSet.
	self assert: installedSuperclass class >> #new equals: sourceForLibrary class >> #new.
	self assert: installedSuperclass >> #initialize equals: sourceForLibrary >> #initialize.
	self assert: installedSuperclass >> #a equals: sourceForLibrary >> #a.
	self assert: installedSuperclass >> #b equals: sourceForLibrary >> #b.
	self assert: installedSuperclass >> #c equals: sourceForLibrary >> #c.
	self deny: (installedSuperclass >> #combined) = (sourceForExistingSuperclass >> #combined).
	self assert: installedSuperclass >> #combined equals: sourceForLibrary >> #combined.
	self assert: installedSuperclass >> #maxVal equals: sourceForLibrary >> #maxVal.
	self assert: installedSuperclass >> #someLiteral equals: sourceForLibrary >> #someLiteral.
	self assert: installedSuperclass >> #xk47 equals: sourceForExistingSuperclass >> #xk47.

	self assert: installedSubclass selectors equals: sourceForExistingSubclass selectors.
	self assert: installedSubclass class selectors equals: sourceForExistingSubclass class selectors.
	self deny: (installedSubclass >> #initialize) = (sourceForExistingSubclass >> #initialize)
		description: 'Methods should be corrected for class shape due to new instance variable in superclass'.
	self deny: (installedSubclass >> #d) = (sourceForExistingSubclass >> #d)
		description: 'Methods should be corrected for class shape due to new instance variable in superclass'.
	self deny: (installedSubclass >> #combined) = (sourceForExistingSubclass >> #combined)
		description: 'Methods should be corrected for class shape due to new instance variable in superclass'.

	self assert: installedSuperclass new a equals: 1.
	self assert: installedSuperclass new b equals: 2.
	self assert: installedSuperclass new c equals: 3.
	self assert: installedSuperclass new combined equals: 123.
	self assert: installedSuperclass new maxVal equals: SmallInteger maxVal.
	self assert: installedSuperclass new someLiteral equals: sourceForLibrary new someLiteral.
	self assert: installedSuperclass new xk47 equals: 'xk47'.

	self assert: installedSubclass new a equals: 1.
	self assert: installedSubclass new b equals: 2.
	self assert: installedSubclass new c equals: 3.
	self assert: installedSubclass new d equals: 4.
	self assert: installedSubclass new combined equals: 124 + 123.
	self assert: installedSubclass new maxVal equals: SmallInteger maxVal.
	self assert: installedSubclass new someLiteral equals: sourceForLibrary new someLiteral.
	self assert: installedSubclass new xk47 equals: 'xk47'.

	Smalltalk at: #DclExample03Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 03' stamp: 'jmv 8/11/2023 10:25:13'!
test03DExistingClassOldIvarMissingShouldKeepShape
	"The library is unaware of one of the instance variables in the existing class.
	Installation should succeed.
	Shape of the class should not change."

	| sourceForExistingSuperclass sourceForExistingSubclass existingSuperclass existingSubclass
	 sourceForLibrarySubclass lib installedSubclass |

	"Create pre-existing version of classes."
	Smalltalk at: #DclExample03Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExistingSuperclass := NoclashABC_DclExample03.
	sourceForExistingSubclass := NoclashABCD_DclExample03Subclass.
	(DynamicCuisLibrary
		withClasses: { sourceForExistingSuperclass. sourceForExistingSubclass }
		classSelectors: { sourceForExistingSuperclass class selectors. sourceForExistingSubclass class selectors }
		instanceSelectors: { sourceForExistingSuperclass selectors. sourceForExistingSubclass selectors }) installLibrary.
	existingSuperclass := Smalltalk at: #DclExample03.
	existingSubclass := Smalltalk at: #DclExample03Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('a' 'b' 'c').
	self assert: existingSubclass allInstVarNames equals: #('a' 'b' 'c' 'd').
	self assert: existingSuperclass new a equals: 1.
	self assert: existingSuperclass new b equals: 2.
	self assert: existingSuperclass new c equals: 3.
	self assert: existingSuperclass new combined equals: 123.
	self assert: existingSubclass new a equals: 1.
	self assert: existingSubclass new b equals: 2.
	self assert: existingSubclass new c equals: 3.
	self assert: existingSubclass new d equals: 4.
	self assert: existingSubclass new combined equals: 1234 + 123.

	"Create library. Class we are installed misses one instance variable."
	sourceForLibrarySubclass := NoclashABD_DclExample03Subclass.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrarySubclass }
		classSelectors: { sourceForLibrarySubclass class selectors }
		instanceSelectors: { sourceForLibrarySubclass selectors }.
	
	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedSubclass := Smalltalk at: #DclExample03Subclass.
	self assert: installedSubclass allInstVarNames equals: #('a' 'b' 'c' 'd').

	Smalltalk at: #DclExample03Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 03' stamp: 'jmv 8/11/2023 10:39:36'!
test03EExistingClassOldIvarMissingShouldConvertNewMethods
	"The library is unaware of one of the instance variables in the existing class.
	Installation should succeed.
	Methods in the subclass that came from the library should be adjusted to access the correct instance variables,
		because they were created unaware of the 'missing' instance variable."

	| sourceForExistingSuperclass sourceForExistingSubclass existingSuperclass existingSubclass
	 sourceForLibrarySubclass lib installedSubclass |

	"Create pre-existing version of classes."
	Smalltalk at: #DclExample03Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExistingSuperclass := NoclashABC_DclExample03.
	sourceForExistingSubclass := NoclashABCD_DclExample03Subclass.
	(DynamicCuisLibrary
		withClasses: { sourceForExistingSuperclass. sourceForExistingSubclass }
		classSelectors: { sourceForExistingSuperclass class selectors. sourceForExistingSubclass class selectors }
		instanceSelectors: { sourceForExistingSuperclass selectors. sourceForExistingSubclass selectors }) installLibrary.
	existingSuperclass := Smalltalk at: #DclExample03.
	existingSubclass := Smalltalk at: #DclExample03Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('a' 'b' 'c').
	self assert: existingSubclass allInstVarNames equals: #('a' 'b' 'c' 'd').
	self assert: existingSuperclass new a equals: 1.
	self assert: existingSuperclass new b equals: 2.
	self assert: existingSuperclass new c equals: 3.
	self assert: existingSuperclass new combined equals: 123.
	self assert: existingSubclass new a equals: 1.
	self assert: existingSubclass new b equals: 2.
	self assert: existingSubclass new c equals: 3.
	self assert: existingSubclass new d equals: 4.
	self assert: existingSubclass new combined equals: 1234 + 123.

	"Create library. Class we are installed misses one instance variable (in the superclass actually).
	Neither class shape should be modified."
	sourceForLibrarySubclass := NoclashABD_DclExample03Subclass.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrarySubclass }
		classSelectors: { sourceForLibrarySubclass class selectors }
		instanceSelectors: { sourceForLibrarySubclass selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedSubclass := Smalltalk at: #DclExample03Subclass.
	self assert: existingSuperclass allInstVarNames equals: #('a' 'b' 'c').
	self assert: installedSubclass allInstVarNames equals: #('a' 'b' 'c' 'd').
	self assert: existingSuperclass new a equals: 1.
	self assert: existingSuperclass new b equals: 2.
	self assert: existingSuperclass new c equals: 3.
	self assert: existingSuperclass new combined equals: 123.
	self assert: installedSubclass new a equals: 1.
	self assert: installedSubclass new b equals: 2.
	self assert: installedSubclass new c equals: 3.
	self assert: installedSubclass new d equals: 4.
	self assert: installedSubclass new combined equals: 124 + 123.

	Smalltalk at: #DclExample03Subclass ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 03' stamp: 'jmv 8/11/2023 10:25:23'!
test03FExistingClassReorderedIVarsShouldConvertNewMethods
	"The library adds an instance variable to a class.
	Installation should succeed.
	The new instance variable will be added after the existing ones.
	Existing methods in the subclass should be converted to access the correct instance variables."

	| sourceForExisting existingClass sourceForLibrary lib installedFromLibrary |

	"Create pre-existing version of the class."
	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExisting := NoclashACB_DclExample03.
	(DynamicCuisLibrary
		withClasses: { sourceForExisting }
		classSelectors: { sourceForExisting class selectors }
		instanceSelectors: { sourceForExisting selectors }) installLibrary.
	existingClass := Smalltalk at: #DclExample03.
	self assert: existingClass allInstVarNames equals: #('a' 'c' 'b').

	"Create library"
	sourceForLibrary := NoclashABC_DclExample03.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedFromLibrary := Smalltalk at: #DclExample03.

	self assert: installedFromLibrary allInstVarNames equals: #('a' 'c' 'b').
	self assert: installedFromLibrary new initialize2 a equals: 1.
	self assert: installedFromLibrary new initialize2 b equals: 2.
	self assert: installedFromLibrary new initialize2 c equals: 3.
	self assert: installedFromLibrary new initialize2 combined equals: 123.

	Smalltalk at: #DclExample03 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 04' stamp: 'jmv 8/11/2023 10:25:35'!
test04AExistingClassNewIVarsAlreadyInSuper
	"The library includes an instance variable that is already defined in a superclass in the image.
	Installation should fail."

	| sourceForExisting existing sourceForLibrarySuperclass sourceForLibrarySubclass lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample04 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExisting := NoclashABC_DclExample04.
	(DynamicCuisLibrary
		withClasses: { sourceForExisting }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.
	existing := Smalltalk at: #DclExample04.
	self assert: existing instVarNames equals: sourceForExisting instVarNames.

	"Create library"
	sourceForLibrarySuperclass := NoclashAC_DclExample04.
	sourceForLibrarySubclass := NoclashB_DclExample04Sub.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrarySuperclass.  sourceForLibrarySubclass }
		classSelectors: { {}. {} }
		instanceSelectors: { {}. {} }.

	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample04 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 04' stamp: 'jmv 8/11/2023 10:26:24'!
test04BExistingClassNewIVarsAlreadyInSub
	"The library includes an instance variable that is already defined in a subclass in the image.
	Installation should fail."

	| sourceForExistingSuperclass sourceForExistingSubclass existingSuperclass existingSubclass sourceForLibrary lib |

	"Create pre-existing version of the classes"
	Smalltalk at: #DclExample04Sub ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample04 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExistingSuperclass := NoclashAC_DclExample04.
	sourceForExistingSubclass := NoclashB_DclExample04Sub.
	(DynamicCuisLibrary
		withClasses: { sourceForExistingSuperclass.  sourceForExistingSubclass }
		classSelectors: { {}. {} }
		instanceSelectors: { {}. {} }) installLibrary.
	existingSuperclass := Smalltalk at: #DclExample04.
	existingSubclass := Smalltalk at: #DclExample04Sub.
	self assert: existingSuperclass instVarNames equals: sourceForExistingSuperclass instVarNames.
	self assert: existingSubclass instVarNames equals: sourceForExistingSubclass instVarNames.

	"Create library"
	sourceForLibrary := NoclashABC_DclExample04.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { {} }
		instanceSelectors: { {} }.

	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample04Sub ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample04 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 10:26:20'!
test05ADifferentSuperclass
	"Existing class in different hierarchy.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { Noclash_DclExample05Super.  NoclashSub_DclExample05 }
		classSelectors: { {}. {} }
		instanceSelectors: { {}. {} }.

	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 10:25:59'!
test05BDifferentSuperclass
	"Existing class in different hierarchy.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashProtoObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { Noclash_DclExample05Super.  NoclashSub_DclExample05 }
		classSelectors: { {}. {} }
		instanceSelectors: { {}. {} }.

	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 10:26:04'!
test05CDifferentSuperclass
	"Existing class in different hierarchy.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample05Super ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { Noclash_DclExample05Super.  NoclashSub_DclExample05 }
		classSelectors: { {}. {} }
		instanceSelectors: { {}. {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.

	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample05Super ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 10:26:08'!
test05DDifferentSuperclass
	"Existing class in different hierarchy.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample05Super ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { Noclash_DclExample05Super.  NoclashSub_DclExample05 }
		classSelectors: { {}. {} }
		instanceSelectors: { {}. {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashProtoObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.

	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk at: #DclExample05Super ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 10:26:13'!
test05EDifferentSuperclass
	"Existing class in different hierarchy.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.
	(Smalltalk at: #DclExample05) superclass: nil.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.

	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 16:36:03'!
test05FDifferentType
	"Existing class is different type.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashIndexed_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWeak_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 16:36:15'!
test05GDifferentType
	"Existing class is different type.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashIndexed_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWeak_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 16:36:21'!
test05HDifferentType
	"Existing class is different type.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashIndexed_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWeak_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 16:36:26'!
test05IDifferentType
	"Existing class is different type.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashDoubleByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashIndexed_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWeak_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 16:36:31'!
test05JDifferentType
	"Existing class is different type.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashIndexed_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWeak_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 16:36:37'!
test05KDifferentType
	"Existing class is different type.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashDoubleWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashIndexed_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWeak_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 05' stamp: 'jmv 8/11/2023 16:36:55'!
test05LDifferentType
	"Existing class is different type.
	Installation should fail."

	| lib |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	(DynamicCuisLibrary
		withClasses: { NoclashWeak_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }) installLibrary.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashObject_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashIndexed_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleByte_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { NoclashDoubleWord_DclExample05 }
		classSelectors: { {} }
		instanceSelectors: { {} }.
	self deny: lib canBeInstalled.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	Smalltalk at: #DclExample05 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'tests 06' stamp: 'jmv 8/14/2023 15:48:32'!
test06MissingGlobal
	"Verify that unknown variables are created as Globals (not as Undeclared), with a nil value."

	| rsg lib installedFromLibrary |
	Smalltalk at: #DclExample06 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk removeKey: #DclExample06Global ifAbsent: nil.
	Smalltalk cleanOutUndeclared.

	Smalltalk at: #DclExample06Global put: 123.4.
	rsg := Smalltalk at: #LogCompilerUse.
	Smalltalk at: #LogCompilerUse put: false.
	Object subclass: #DclExample06
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'TestDynamicCuisLibraries-Transient'.
	(Smalltalk at: #DclExample06) compile: 'a1 ^DclExample06Global'.
	Smalltalk at: #LogCompilerUse put: rsg.
	self assert: (Smalltalk at: #DclExample06) new a1 = 123.4.	

	"Create library"
	lib := DynamicCuisLibrary
		withClasses: { #DclExample06 }
		classSelectors: { {} }
		instanceSelectors: { {#a1} }.

	(Smalltalk at: #DclExample06) removeFromSystemUnlogged.
	Smalltalk removeKey: #DclExample06Global.
	Smalltalk cleanOutUndeclared.

	self deny: (Smalltalk includesKey: #DclExample06Global) description: 'Precondition for this test failed. Please check test itself.'.

	self assert: lib canBeInstalled.
	lib installLibrary.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedFromLibrary := Smalltalk at: #DclExample06.
	self assert: (Smalltalk includesKey: #DclExample06Global) description: 'Global should have been created automatically on method materialization'.
	self assert: installedFromLibrary new a1 equals: nil.	
	Smalltalk at: #DclExample06Global put: 1234.56.
	self assert: installedFromLibrary new a1 equals: 1234.56.	

	Smalltalk at: #DclExample06 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	Smalltalk removeKey: #DclExample06Global ifAbsent: nil.
	Smalltalk cleanOutUndeclared.! !

!DclTest methodsFor: 'tests 06' stamp: 'jmv 8/11/2023 12:34:16'!
test06NewClassVar
	| sourceForExisting existing sourceForLibrary lib anInstance installedFromLibrary |

	"Create pre-existing version of the class"
	Smalltalk at: #DclExample06 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].
	sourceForExisting := NoclashABC_DclExample06.
	(DynamicCuisLibrary
		withClasses: { sourceForExisting }
		classSelectors: { sourceForExisting class selectors }
		instanceSelectors: { sourceForExisting selectors }) installLibrary.
	existing := Smalltalk at: #DclExample06.
	self assert: existing instVarNames equals: sourceForExisting instVarNames.
	self assert: existing selectors equals: sourceForExisting selectors.
	self assert: existing class selectors equals: existing class selectors.
	self assert: existing >> #initialize equals: sourceForExisting >> #initialize.
	self assert: existing >> #a equals: sourceForExisting >> #a.
	self assert: existing >> #b equals: sourceForExisting >> #b.
	self assert: existing >> #c equals: sourceForExisting >> #c.
	self assert: existing >> #combined equals: sourceForExisting >> #combined.
	self assert: existing >> #maxVal equals: sourceForExisting >> #maxVal.
	self assert: existing >> #someLiteral equals: sourceForExisting >> #someLiteral.

	anInstance := existing new.
	self assert: anInstance a equals: 1.
	self assert: anInstance b equals: 2.
	self assert: anInstance c equals: 3.
	self assert: anInstance combined equals: 123.
	self assert: anInstance maxVal equals: SmallInteger maxVal.
	self assert: anInstance someLiteral equals: NoclashABC_DclExample06 new someLiteral.

	"Create library"
	sourceForLibrary := NoclashABCclasVarZZ_DclExample06.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.

	self assert: lib canBeInstalled.
	lib installLibrary.
	"Ensure that the instance is still around"
	self assert: anInstance notNil.
	self deny: CompilerWasUsed description: 'Should not use Compiler. Usually it will not be available.'.

	installedFromLibrary := Smalltalk at: #DclExample06.
	self assert: installedFromLibrary instVarNames equals: sourceForLibrary instVarNames.
	self assert: installedFromLibrary selectors asSet equals:
		(sourceForExisting selectors, sourceForLibrary selectors) asSet.
	self assert: installedFromLibrary class selectors asSet equals:
		(sourceForExisting class selectors, sourceForLibrary class selectors) asSet.
	self assert: installedFromLibrary >> #initialize equals: sourceForExisting >> #initialize.
	self assert: installedFromLibrary >> #a equals: sourceForExisting >> #a.
	self assert: installedFromLibrary >> #b equals: sourceForExisting >> #b.
	self assert: installedFromLibrary >> #c equals: sourceForExisting >> #c.
	self assert: installedFromLibrary >> #combined equals: sourceForExisting >> #combined.
	self assert: installedFromLibrary >> #maxVal equals: sourceForExisting >> #maxVal.
	self assert: installedFromLibrary >> #someLiteral equals: sourceForExisting >> #someLiteral.
	self assert: installedFromLibrary >> #setAndAnswerZZ equals: sourceForLibrary >> #setAndAnswerZZ.

	self assert: installedFromLibrary setAndAnswerZZ2 = 12742.
	self assert: anInstance setAndAnswerZZ = 1274.
	self assert: anInstance a equals: 1.
	self assert: anInstance b equals: 2.
	self assert: anInstance c equals: 3.
	self assert: anInstance combined equals: 123.
	self assert: anInstance maxVal equals: SmallInteger maxVal.
	self assert: anInstance someLiteral equals: NoclashABC_DclExample06 new someLiteral.

	Smalltalk at: #DclExample06 ifPresent: [ :leftover | leftover removeFromSystemUnlogged ].! !

!DclTest methodsFor: 'setUp/tearDown' stamp: 'jmv 8/9/2023 16:09:45'!
setUp
	Smalltalk at: #LogCompilerUse put: true.
	Smalltalk at: #CompilerWasUsed put: false.! !

!DclTest methodsFor: 'setUp/tearDown' stamp: 'jmv 8/8/2023 10:46:31'!
tearDown
	LogCompilerUse := false! !

!DclTest class methodsFor: 'class initialization' stamp: 'jmv 10/19/2023 15:37:34'!
initialize

	Smalltalk at: #LogCompilerUse put: false.
	Smalltalk at: #CompilerWasUsed put: false.! !

!Noclash_CodeModelExperiment methodsFor: 'sample methods' stamp: 'jmv 8/2/2023 16:43:04'!
method01
	"Just for testing.
	"

	| a b1 b2 b3 b4 b5 c d |
	a := Smalltalk.  								"Method literal is a ReadOnlyVariableBinding"
	b1 := Point.  								"Method literal is a ReadOnlyVariableBinding"
	b2 := Point class.  								"Method literal is a ReadOnlyVariableBinding."
	b3 := Smalltalk at: #Point.  								"Additional method literal is the symbol #Point"
	b4 := `Smalltalk at: #Point`. 								"Method literal is class Point"
	b5 := `(Smalltalk at: #Point) class`. 								"Method literal is metaclass Point"
	c := Display.  								"Method literal is an Association"
	d := `Smalltalk at: #Display`.  								"Method literal is the instance of DisplayScreen"
	^ {a. b1. b2. b3. b4. b5. c. d}.
"Qué cosas pueden ser literal de un metodo?
+ isLiteral == true (Booleans, Numbers, String, Arrays, NumericArrays...
+ Association / ReadOnlyVariableBinding (Globales, classvars, poolDict elements)
+ literales arbitrarios (`code`) - PERO NO TENEMOS COMPILADOR!!!!!!!!!!!!!!!!!! 
	Usar `ReferenceStream streamedRepresentationOf: aLiteral` y después `ReferenceStream unStream: aByteArray`?"! !

!Noclash_PointCodeModelExperiment methodsFor: 'accessing' stamp: 'jmv 8/2/2023 16:44:39'!
x
^x! !

!Noclash_PointCodeModelExperiment methodsFor: 'accessing' stamp: 'jmv 8/2/2023 16:44:39'!
y
^y! !

!Noclash_PointCodeModelExperiment methodsFor: 'private - initialization' stamp: 'jmv 8/2/2023 16:44:39'!
initializePvtX: xValue y: yValue
	"Points are immutable."
	x := xValue.
	y := yValue! !

!Noclash_PointCodeModelExperiment methodsFor: 'printing' stamp: 'jmv 8/2/2023 16:44:39'!
printOn: aStream 
	"The receiver prints on aStream in terms of infix notation."

	x printOn: aStream.
	aStream nextPut: $@.
	y negative ifTrue: [ aStream space ].
	y printOn: aStream! !

!Noclash_PointCodeModelExperiment class methodsFor: 'instance creation' stamp: 'jmv 8/2/2023 16:44:39'!
x: anX y: anY
	"Answer an instance of me with supplied coordinates."

	^self new initializePvtX: anX y: anY! !

!Noclash_DclExample01 methodsFor: 'accessing' stamp: 'jmv 8/2/2023 14:25:39'!
a
	^a! !

!Noclash_DclExample01 methodsFor: 'accessing' stamp: 'jmv 8/2/2023 14:25:42'!
b
	^b! !

!Noclash_DclExample01 methodsFor: 'accessing' stamp: 'jmv 8/2/2023 14:25:45'!
c
	^c! !

!Noclash_DclExample01 methodsFor: 'initialization' stamp: 'jmv 8/2/2023 14:25:23'!
initialize
	a := 1.
	b := 2.
	c := 3.! !

!Noclash_DclExample01 methodsFor: 'other' stamp: 'jmv 8/2/2023 14:53:02'!
combined
	^ self a * 10 + self b * 10 + self c.! !

!Noclash_DclExample01 methodsFor: 'other' stamp: 'jmv 8/2/2023 14:37:41'!
maxVal
	^SmallInteger maxVal.! !

!Noclash_DclExample01 methodsFor: 'other' stamp: 'jmv 8/2/2023 14:39:07'!
someLiteral
	^#(1 31 #aSymbol #otherSymbol)! !

!Noclash_DclExample01 class methodsFor: 'instance creation' stamp: 'jmv 8/2/2023 14:25:08'!
new
	^ self basicNew initialize! !

!Noclash_DclExample01 class methodsFor: 'other' stamp: 'jmv 10/24/2023 09:45:30'!
combinedCIvars
	^aa *10 + bb! !

!Noclash_DclExample01 class methodsFor: 'other' stamp: 'jmv 8/2/2023 14:44:20'!
processor
	^Processor! !

!Noclash_DclExample01 class methodsFor: 'other' stamp: 'jmv 10/24/2023 09:44:50'!
setClassIVars
	aa := 5.
	bb := 7.! !

!NoclashExisting_DclExample02 methodsFor: 'accessing' stamp: 'jmv 8/7/2023 18:10:54'!
b
	^ b.! !

!NoclashExisting_DclExample02 methodsFor: 'initialization' stamp: 'jmv 8/7/2023 18:10:54'!
initialize
	a _ 1.
	b _ 2.
	c _ 3.! !

!NoclashExisting_DclExample02 methodsFor: 'other' stamp: 'jmv 8/8/2023 15:52:02'!
combined
	^ 47! !

!NoclashExisting_DclExample02 methodsFor: 'other' stamp: 'jmv 10/24/2023 12:23:50'!
maxVal
	^ SmallInteger maxVal-1.! !

!NoclashExisting_DclExample02 methodsFor: 'other' stamp: 'jmv 10/24/2023 12:25:06'!
someLiteral
	^#(22 33)! !

!NoclashExisting_DclExample02 class methodsFor: 'instance creation' stamp: 'jmv 8/7/2023 18:10:54'!
new
	^ self basicNew initialize.! !

!NoclashExisting_DclExample02 class methodsFor: 'other' stamp: 'jmv 10/24/2023 12:28:25'!
combinedCIvars
	^aa + bb! !

!NoclashExisting_DclExample02 class methodsFor: 'other' stamp: 'jmv 10/24/2023 12:26:21'!
processor
	^ nil.! !

!NoclashExisting_DclExample02 class methodsFor: 'other' stamp: 'jmv 10/24/2023 12:28:55'!
setClassIVars
	aa := 3.
	bb := 4.! !

!Noclash_DclExample02 methodsFor: 'accessing' stamp: 'jmv 8/2/2023 14:45:23'!
a
	^a! !

!Noclash_DclExample02 methodsFor: 'accessing' stamp: 'jmv 8/2/2023 14:45:23'!
b
	^b! !

!Noclash_DclExample02 methodsFor: 'accessing' stamp: 'jmv 8/2/2023 14:54:12'!
c
	^c! !

!Noclash_DclExample02 methodsFor: 'initialization' stamp: 'jmv 8/2/2023 14:45:23'!
initialize
	a := 1.
	b := 2.
	c := 3.! !

!Noclash_DclExample02 methodsFor: 'other' stamp: 'jmv 8/2/2023 14:53:05'!
combined
	^ self a * 10 + self b * 10 + self c.! !

!Noclash_DclExample02 methodsFor: 'other' stamp: 'jmv 8/2/2023 14:45:23'!
maxVal
	^SmallInteger maxVal.! !

!Noclash_DclExample02 methodsFor: 'other' stamp: 'jmv 8/2/2023 14:45:23'!
someLiteral
	^#(1 31 #aSymbol #otherSymbol)! !

!Noclash_DclExample02 class methodsFor: 'instance creation' stamp: 'jmv 8/2/2023 14:45:23'!
new
	^ self basicNew initialize! !

!Noclash_DclExample02 class methodsFor: 'other' stamp: 'jmv 10/24/2023 09:50:43'!
combinedCIvars
	^aa *10 + bb! !

!Noclash_DclExample02 class methodsFor: 'other' stamp: 'jmv 8/2/2023 14:45:23'!
processor
	^Processor! !

!Noclash_DclExample02 class methodsFor: 'other' stamp: 'jmv 10/24/2023 09:50:39'!
setClassIVars
	aa := 5.
	bb := 7.! !

!NoclashABC_DclExample03 methodsFor: 'accessing' stamp: 'jmv 8/4/2023 15:47:22'!
a
	^a! !

!NoclashABC_DclExample03 methodsFor: 'accessing' stamp: 'jmv 8/4/2023 15:47:22'!
b
	^b! !

!NoclashABC_DclExample03 methodsFor: 'accessing' stamp: 'jmv 8/4/2023 15:47:22'!
c
	^c! !

!NoclashABC_DclExample03 methodsFor: 'initialization' stamp: 'jmv 8/4/2023 15:47:22'!
initialize
	a := 1.
	b := 2.
	c := 3.! !

!NoclashABC_DclExample03 methodsFor: 'other' stamp: 'jmv 8/4/2023 16:51:03'!
combined
	 ^ a * 10 + b * 10 + c.! !

!NoclashABC_DclExample03 methodsFor: 'other' stamp: 'jmv 8/4/2023 15:47:22'!
maxVal
	^SmallInteger maxVal.! !

!NoclashABC_DclExample03 methodsFor: 'other' stamp: 'jmv 8/4/2023 15:47:22'!
someLiteral
	^#(1 31 #aSymbol #otherSymbol)! !

!NoclashABC_DclExample03 class methodsFor: 'instance creation' stamp: 'jmv 8/4/2023 15:47:22'!
new
	^ self basicNew initialize! !

!NoclashABCD_DclExample03Subclass methodsFor: 'accessing' stamp: 'jmv 8/4/2023 16:42:33'!
d
	^d! !

!NoclashABCD_DclExample03Subclass methodsFor: 'initialization' stamp: 'jmv 8/4/2023 16:50:20'!
initialize
	super initialize.
	d := 4.! !

!NoclashABCD_DclExample03Subclass methodsFor: 'other' stamp: 'jmv 8/10/2023 11:07:31'!
combined
	 ^ a * 10 + b * 10 + c * 10 + d + super combined! !

!NoclashAB_DclExample03 methodsFor: 'accessing' stamp: 'jmv 8/4/2023 15:47:11'!
b
	^ b.! !

!NoclashAB_DclExample03 methodsFor: 'initialization' stamp: 'jmv 8/4/2023 15:47:11'!
initialize
	a := 7.
	b := 9.! !

!NoclashAB_DclExample03 methodsFor: 'other' stamp: 'jmv 8/4/2023 16:51:12'!
combined
	 ^ a * 10 + b! !

!NoclashAB_DclExample03 methodsFor: 'other' stamp: 'jmv 8/11/2023 12:37:03'!
xk47
	^'xk47'! !

!NoclashAB_DclExample03 class methodsFor: 'instance creation' stamp: 'jmv 8/4/2023 15:47:11'!
new
	^ self basicNew initialize.! !

!NoclashABD_DclExample03Subclass methodsFor: 'accessing' stamp: 'jmv 8/4/2023 16:40:26'!
d
	^d! !

!NoclashABD_DclExample03Subclass methodsFor: 'initialization' stamp: 'jmv 8/10/2023 11:26:59'!
initialize
	super initialize.
	d := 4.! !

!NoclashABD_DclExample03Subclass methodsFor: 'other' stamp: 'jmv 8/10/2023 10:48:38'!
combined
	 ^ a * 10 + b * 10 + d + super combined! !

!NoclashACB_DclExample03 methodsFor: 'initialization' stamp: 'jmv 8/10/2023 12:05:25'!
initialize2
	a := 1.
	b := 2.
	c := 3.! !

!NoclashABCclasVarZZ_DclExample06 methodsFor: 'other' stamp: 'jmv 8/11/2023 10:53:44'!
setAndAnswerZZ
	ZZ := 1274.
	^ZZ! !

!NoclashABCclasVarZZ_DclExample06 class methodsFor: 'other' stamp: 'jmv 8/11/2023 12:33:46'!
setAndAnswerZZ2
	ZZ := 12742.
	^ZZ! !

!NoclashABC_DclExample06 methodsFor: 'accessing' stamp: 'jmv 8/11/2023 10:50:24'!
a
	^a! !

!NoclashABC_DclExample06 methodsFor: 'accessing' stamp: 'jmv 8/11/2023 10:50:28'!
b
	^b! !

!NoclashABC_DclExample06 methodsFor: 'accessing' stamp: 'jmv 8/11/2023 10:50:32'!
c
	^c! !

!NoclashABC_DclExample06 methodsFor: 'initialization' stamp: 'jmv 8/11/2023 10:50:51'!
initialize
	a := 1.
	b := 2.
	c := 3.! !

!NoclashABC_DclExample06 methodsFor: 'other' stamp: 'jmv 8/11/2023 10:51:11'!
combined
	 ^ a * 10 + b * 10 + c.! !

!NoclashABC_DclExample06 methodsFor: 'other' stamp: 'jmv 8/11/2023 10:51:14'!
maxVal
	^SmallInteger maxVal.! !

!NoclashABC_DclExample06 methodsFor: 'other' stamp: 'jmv 8/11/2023 10:51:17'!
someLiteral
	^#(1 31 #aSymbol #otherSymbol)! !

!InstructionStream class methodsFor: '*TestDynamicCuisLibraries-monitorCompilerUsage' stamp: 'jmv 8/8/2023 10:59:25'!
new
	LogCompilerUse == true ifTrue: [
		LogCompilerUse := false.
		CompilerWasUsed := true.
		{'---'. self} print.
		thisContext printStack: 10 ].
	^super new.! !

!ContextPart class methodsFor: '*TestDynamicCuisLibraries-monitorCompilerUsage' stamp: 'jmv 8/8/2023 10:59:32'!
newForMethod: aMethod
	"This is the only method for creating new contexts, other than primitive cloning.
	Any other attempts, such as inherited methods like shallowCopy, should be
	avoided or must at least be rewritten to determine the proper size from the
	method being activated.  This is because asking a context its size (even basicSize!!)
	will not return the real object size but only the number of fields currently
	accessible, as determined by stackp."

	LogCompilerUse == true ifTrue: [
		LogCompilerUse := false.
		CompilerWasUsed := true.
		{'---'. self} print.
		thisContext printStack: 10 ].
	^ super basicNew: aMethod frameSize! !

!UnhandledError methodsFor: '*TestDynamicCuisLibraries-monitorCompilerUsage' stamp: 'jmv 8/8/2023 10:55:48'!
devDefaultAction

	LogCompilerUse := false.
	Processor activeProcess
		debugContext: exception signalerContext
		title: exception description.! !

!Compiler class methodsFor: '*TestDynamicCuisLibraries-monitorCompilerUsage' stamp: 'jmv 8/8/2023 10:59:40'!
new
	LogCompilerUse == true ifTrue: [
		LogCompilerUse := false.
		CompilerWasUsed := true.
		{'---'. self} print.
		thisContext printStack: 15 ].
	^ super new parser: self parserClass new.! !

!Scanner class methodsFor: '*TestDynamicCuisLibraries-monitorCompilerUsage' stamp: 'jmv 8/8/2023 10:59:43'!
new
	LogCompilerUse == true ifTrue: [
		LogCompilerUse := false.
		CompilerWasUsed := true.
		{'---'. self} print.
		thisContext printStack: 10 ].
	^self basicNew initScanner! !

!ParseNode class methodsFor: '*TestDynamicCuisLibraries-monitorCompilerUsage' stamp: 'jmv 8/8/2023 10:59:45'!
new
	LogCompilerUse == true ifTrue: [
		LogCompilerUse := false.
		CompilerWasUsed := true.
		{'---'. self} print.
		thisContext printStack: 10 ].
	LdInstType ifNil: [
		ParseNode initClassCachedState.
		VariableNode initClassCachedState ].
	^super new! !

!MessageNode class methodsFor: '*TestDynamicCuisLibraries-monitorCompilerUsage' stamp: 'jmv 8/8/2023 10:59:48'!
new
	LogCompilerUse == true ifTrue: [
		LogCompilerUse := false.
		CompilerWasUsed := true.
		{'---'. self} print.
		thisContext printStack: 10 ].
	MacroSelectors ifNil: [ self initClassCachedState ].
	^super new! !
DclTest initialize!
