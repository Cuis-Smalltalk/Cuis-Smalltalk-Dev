'From Cuis 6.0 [latest update: #6068] on 2 November 2023 at 10:43:17 am'!
'Description '!
!provides: 'Bootstrap' 1 33!
!requires: 'DynamicCuisLibraries' 1 22 nil!
SystemOrganization addCategory: #'Bootstrap-Smalltalk Image Builders'!
SystemOrganization addCategory: #'Bootstrap-Foreign Objects'!


!classDefinition: #ImageBuilder category: #'Bootstrap-Smalltalk Image Builders'!
Object subclass: #ImageBuilder
	instanceVariableNames: 'isBigEndian classes methods baseAddressOfImage nextFreeAddressOfImage lastHash firstForeignObject lastForeignObject foreignObjectsMap specialObjectsArrayForeignObject globalVariables symbols processorBindingForeignObject smalltalkForeignObject displayForeignObject startUpMethod'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Smalltalk Image Builders'!
!classDefinition: 'ImageBuilder class' category: #'Bootstrap-Smalltalk Image Builders'!
ImageBuilder class
	instanceVariableNames: ''!

!classDefinition: #SpurImageBuilder category: #'Bootstrap-Smalltalk Image Builders'!
ImageBuilder subclass: #SpurImageBuilder
	instanceVariableNames: 'classTableRoots lastClassIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Smalltalk Image Builders'!
!classDefinition: 'SpurImageBuilder class' category: #'Bootstrap-Smalltalk Image Builders'!
SpurImageBuilder class
	instanceVariableNames: ''!

!classDefinition: #Spur32ImageBuilder category: #'Bootstrap-Smalltalk Image Builders'!
SpurImageBuilder subclass: #Spur32ImageBuilder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Smalltalk Image Builders'!
!classDefinition: 'Spur32ImageBuilder class' category: #'Bootstrap-Smalltalk Image Builders'!
Spur32ImageBuilder class
	instanceVariableNames: ''!

!classDefinition: #Spur64ImageBuilder category: #'Bootstrap-Smalltalk Image Builders'!
SpurImageBuilder subclass: #Spur64ImageBuilder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Smalltalk Image Builders'!
!classDefinition: 'Spur64ImageBuilder class' category: #'Bootstrap-Smalltalk Image Builders'!
Spur64ImageBuilder class
	instanceVariableNames: ''!

!classDefinition: #V3ImageBuilder category: #'Bootstrap-Smalltalk Image Builders'!
ImageBuilder subclass: #V3ImageBuilder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Smalltalk Image Builders'!
!classDefinition: 'V3ImageBuilder class' category: #'Bootstrap-Smalltalk Image Builders'!
V3ImageBuilder class
	instanceVariableNames: ''!

!classDefinition: #ForeignObject category: #'Bootstrap-Foreign Objects'!
Object subclass: #ForeignObject
	instanceVariableNames: 'representedObject nameOfClassToInstantiate'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'ForeignObject class' category: #'Bootstrap-Foreign Objects'!
ForeignObject class
	instanceVariableNames: ''!

!classDefinition: #AllocatedForeignObject category: #'Bootstrap-Foreign Objects'!
ForeignObject subclass: #AllocatedForeignObject
	instanceVariableNames: 'oop identityHash nextInList'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'AllocatedForeignObject class' category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject class
	instanceVariableNames: ''!

!classDefinition: #BoxedFloat64ForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #BoxedFloat64ForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'BoxedFloat64ForeignObject class' category: #'Bootstrap-Foreign Objects'!
BoxedFloat64ForeignObject class
	instanceVariableNames: ''!

!classDefinition: #ClassForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #ClassForeignObject
	instanceVariableNames: 'metaclassForeignObject methodForeignObjects subclassForeignObjects methodDictForeignObject instanceVariablesForeignObject subclassesForeignObject nameForeignObject classPoolForeignObject sharedPoolsForeignObject'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'ClassForeignObject class' category: #'Bootstrap-Foreign Objects'!
ClassForeignObject class
	instanceVariableNames: ''!

!classDefinition: #CompiledMethodForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #CompiledMethodForeignObject
	instanceVariableNames: 'methodClassForeignObject'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'CompiledMethodForeignObject class' category: #'Bootstrap-Foreign Objects'!
CompiledMethodForeignObject class
	instanceVariableNames: ''!

!classDefinition: #Indexed16BitForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #Indexed16BitForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'Indexed16BitForeignObject class' category: #'Bootstrap-Foreign Objects'!
Indexed16BitForeignObject class
	instanceVariableNames: ''!

!classDefinition: #Indexed32BitForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #Indexed32BitForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'Indexed32BitForeignObject class' category: #'Bootstrap-Foreign Objects'!
Indexed32BitForeignObject class
	instanceVariableNames: ''!

!classDefinition: #Indexed64BitForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #Indexed64BitForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'Indexed64BitForeignObject class' category: #'Bootstrap-Foreign Objects'!
Indexed64BitForeignObject class
	instanceVariableNames: ''!

!classDefinition: #Indexed8BitForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #Indexed8BitForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'Indexed8BitForeignObject class' category: #'Bootstrap-Foreign Objects'!
Indexed8BitForeignObject class
	instanceVariableNames: ''!

!classDefinition: #IndexedOopsForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #IndexedOopsForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'IndexedOopsForeignObject class' category: #'Bootstrap-Foreign Objects'!
IndexedOopsForeignObject class
	instanceVariableNames: ''!

!classDefinition: #IvarsAndIndexedOopsForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #IvarsAndIndexedOopsForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'IvarsAndIndexedOopsForeignObject class' category: #'Bootstrap-Foreign Objects'!
IvarsAndIndexedOopsForeignObject class
	instanceVariableNames: ''!

!classDefinition: #IvarsForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #IvarsForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'IvarsForeignObject class' category: #'Bootstrap-Foreign Objects'!
IvarsForeignObject class
	instanceVariableNames: ''!

!classDefinition: #LargeIntegerForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #LargeIntegerForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'LargeIntegerForeignObject class' category: #'Bootstrap-Foreign Objects'!
LargeIntegerForeignObject class
	instanceVariableNames: ''!

!classDefinition: #MetaclassForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #MetaclassForeignObject
	instanceVariableNames: 'thisClassForeignObject methodForeignObjects methodDictForeignObject instanceVariablesForeignObject'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'MetaclassForeignObject class' category: #'Bootstrap-Foreign Objects'!
MetaclassForeignObject class
	instanceVariableNames: ''!

!classDefinition: #MethodContextForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #MethodContextForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'MethodContextForeignObject class' category: #'Bootstrap-Foreign Objects'!
MethodContextForeignObject class
	instanceVariableNames: ''!

!classDefinition: #MethodDictionaryForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #MethodDictionaryForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'MethodDictionaryForeignObject class' category: #'Bootstrap-Foreign Objects'!
MethodDictionaryForeignObject class
	instanceVariableNames: ''!

!classDefinition: #Spur32FreeListsForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #Spur32FreeListsForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'Spur32FreeListsForeignObject class' category: #'Bootstrap-Foreign Objects'!
Spur32FreeListsForeignObject class
	instanceVariableNames: ''!

!classDefinition: #Spur64FreeListsForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #Spur64FreeListsForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'Spur64FreeListsForeignObject class' category: #'Bootstrap-Foreign Objects'!
Spur64FreeListsForeignObject class
	instanceVariableNames: ''!

!classDefinition: #SpurPseudoArrayForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #SpurPseudoArrayForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'SpurPseudoArrayForeignObject class' category: #'Bootstrap-Foreign Objects'!
SpurPseudoArrayForeignObject class
	instanceVariableNames: ''!

!classDefinition: #WeakIndexedOopsForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #WeakIndexedOopsForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'WeakIndexedOopsForeignObject class' category: #'Bootstrap-Foreign Objects'!
WeakIndexedOopsForeignObject class
	instanceVariableNames: ''!

!classDefinition: #WeakIvarsForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #WeakIvarsForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'WeakIvarsForeignObject class' category: #'Bootstrap-Foreign Objects'!
WeakIvarsForeignObject class
	instanceVariableNames: ''!

!classDefinition: #ZeroSizeForeignObject category: #'Bootstrap-Foreign Objects'!
AllocatedForeignObject subclass: #ZeroSizeForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'ZeroSizeForeignObject class' category: #'Bootstrap-Foreign Objects'!
ZeroSizeForeignObject class
	instanceVariableNames: ''!

!classDefinition: #ImmediateForeignObject category: #'Bootstrap-Foreign Objects'!
ForeignObject subclass: #ImmediateForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'ImmediateForeignObject class' category: #'Bootstrap-Foreign Objects'!
ImmediateForeignObject class
	instanceVariableNames: ''!

!classDefinition: #ImmediateCharacterForeignObject category: #'Bootstrap-Foreign Objects'!
ImmediateForeignObject subclass: #ImmediateCharacterForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'ImmediateCharacterForeignObject class' category: #'Bootstrap-Foreign Objects'!
ImmediateCharacterForeignObject class
	instanceVariableNames: ''!

!classDefinition: #SmallFloat64ForeignObject category: #'Bootstrap-Foreign Objects'!
ImmediateForeignObject subclass: #SmallFloat64ForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'SmallFloat64ForeignObject class' category: #'Bootstrap-Foreign Objects'!
SmallFloat64ForeignObject class
	instanceVariableNames: ''!

!classDefinition: #SmallIntegerForeignObject category: #'Bootstrap-Foreign Objects'!
ImmediateForeignObject subclass: #SmallIntegerForeignObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bootstrap-Foreign Objects'!
!classDefinition: 'SmallIntegerForeignObject class' category: #'Bootstrap-Foreign Objects'!
SmallIntegerForeignObject class
	instanceVariableNames: ''!


!ImageBuilder commentStamp: '<historical>' prior: 0!
My sub instances can build new image files, from a set of Foreign Objects.

Cuis image files are usually Little Endian Spur64, but could also be big endian. Images intended for 32 bit systems and VMs are in Spur32 format.

ImageBuilder could be extended to support other image formats, if a suitable VM exists for them, For instance, it is entirely possible to complete V3ImageBuilder and make it functional.

- foreignObjects: an IdentityDictionary that holds an appropriate ForeignObject for each object we want to include.
- firstForeignObject: the start of the linked list of ForeignObjects (they have an ivar for next in list).
- lastForeignObject: handy when adding new ones to the list.!

!SpurImageBuilder commentStamp: '<historical>' prior: 0!
My sub instances can build new image files, in one of the Spur format variants, from a set of Foreign Objects.!

!Spur32ImageBuilder commentStamp: '<historical>' prior: 0!
My instances can save 32 bit Spur images.!

!Spur64ImageBuilder commentStamp: '<historical>' prior: 0!
My instances can save 64 bit Spur images.!

!V3ImageBuilder commentStamp: '<historical>' prior: 0!
My instances can build new image files, in the V3 (a.k.a. pre-Spur) image format, from a set of Foreign Objects.!

!ForeignObject commentStamp: '<historical>' prior: 0!
Foreign objects are representatives of actual objects we may want to include in a new Smalltalk image. Subclasses represent the different kind of classes that are possible.!

!AllocatedForeignObject commentStamp: '<historical>' prior: 0!
AllocatedForeignObjects represent objects that are allocated in the heap.!

!ClassForeignObject commentStamp: '<historical>' prior: 0!
representedObject is ClassSpec.
We are  added to the ImageBuilder foreignObjects dictionary, with the ClassSpec as key. This is useful for building the class hierarchy. But we are added to the foreignObjects list.
Any reference in a serialized object to a class must be handled specially.
No references to any real Class. This means we don't depend on the representation used in the image we're running in. We just need to know how to create the class in the format required for the new image.!

!CompiledMethodForeignObject commentStamp: '<historical>' prior: 0!
representedObject is MethodSpec.
We're not added to the ImageBuilder foreignObjects dictionary. But we are added to the foreignObjects list.
Any reference in a serialized object to a compiled method must be handled specially.
No references to any real CompiledMethod. This means we don't depend on the representation used in the image we're running in. We just need to know how to create method in the format required for the new image.!

!LargeIntegerForeignObject commentStamp: '<historical>' prior: 0!
Both for LargePositiveInteger and LargeNegativeInteger.!

!MetaclassForeignObject commentStamp: '<historical>' prior: 0!
representedObject is ClassSpec.
We're not added to the ImageBuilder foreignObjects dictionary. But we are added to the foreignObjects list.
Any reference in a serialized object to a metaclass must be handled specially.
No references to any real Metaclass. This means we don't depend on the representation used in the image we're running in. We just need to know how to create the Metaclass in the format required for the new image.!

!MethodDictionaryForeignObject commentStamp: '<historical>' prior: 0!
representedObject is nil.
We're not added to the ImageBuilder foreignObjects dictionary. But we are added to the foreignObjects list.
Any reference in a serialized object to a MethodDictionary must be handled specially and is most likely an error condition.
We do know all our MethodSpecs via their CompiledMethodForeignObjects.
No references to any object in the running image. This means we don't depend on the representation used in the image we're running in. We just need to know how to create a MethodDictionary in the format required for the new image.
Only when it is time to serialize ourselves in a new image, we gather all our methods and build suitable state for our new instance. This means that only at that moment we do #addForeignObjectFor: for the array.!

!Spur32FreeListsForeignObject commentStamp: '<historical>' prior: 0!
The Spur FreeLists object is pretty special. It is very much like a regular Smalltalk Array. But it is used by the VM to hold machine pointers, not oops. So, it is a numeric array, closer to ByteArray. Additionally, it is not visible from the Smalltalk side. So, it is not an instance of any class.

There are two possible variants, for Spur64 and Spur32. It has a number of slots (64 or 32). Its format is 9 (64 bits indexable) or 10 (32 bits indexable). Hash is set to 0. ClassIndex is set to 19 or 18. These don't refer to any class!! See VMMaker SpurMemoryManager>>#sixtyFourBitLongsClassIndexPun and VMMaker SpurMemoryManager>>#thirtyTwoBitLongsClassIndexPun.

The purpose of this class is to create correctly these objects in a new Spur image.

'/Users/juanvuletich/Cuis-Smalltalk/Cuis-Smalltalk-Dev/Cuis6.0-5797.image'  asFileEntry readStreamDo: [ :fs | fs binary. cuisSpur64ImageFirstBytes := fs next: 1000 ].
'/Users/juanvuletich/Cuis-Smalltalk/Cuis-Smalltalk-Dev/Cuis6.0-5797-32.image'  asFileEntry readStreamDo: [ :fs | fs binary. cuisSpur32ImageFirstBytes := fs next: 1000 ].

(cuisSpur64ImageFirstBytes uint64At: 128+1+(8*6) bigEndian: false) hex. '16r 40  000000    09  000013' 
slots: 16r40=64
hash:0
format: 9 (64 bits indexable)
classIndex: 16r13=19 (VMMaker SpurMemoryManager>>#sixtyFourBitLongsClassIndexPun, not a class!!)
Siguen 64*8=512 bytes en cero. Despues viene el HiddenRoots, que es un objeto de mas de 255 slots!!
(cuisSpur64ImageFirstBytes uint64At: 128+1+(8*6)+8+512 bigEndian: false) hex.


(cuisSpur32ImageFirstBytes uint64At: 64+1+(8*6) bigEndian: false) hex. '16r 20  000000   0A  000012' 
slots: 16r20=32
hash=0
format: 0A (32 bits indexable)
classIndex: 16r12=18 (VMMaker SpurMemoryManager>>#thirtyTwoBitLongsClassIndexPun, not a class!!)
siguen 32*4=128 bytes en cero. Despues viene el HiddenRoots, que es un objeto de mas de 255 slots!!
(cuisSpur32ImageFirstBytes uint64At: 64+1+(8*6)+8+128 bigEndian: false) hex.!

!Spur64FreeListsForeignObject commentStamp: '<historical>' prior: 0!
The Spur FreeLists object is pretty special. It is very much like a regular Smalltalk Array. But it is used by the VM to hold machine pointers, not oops. So, it is a numeric array, closer to ByteArray. Additionally, it is not visible from the Smalltalk side. So, it is not an instance of any class.

There are two possible variants, for Spur64 and Spur32. It has a number of slots (64 or 32). Its format is 9 (64 bits indexable) or 10 (32 bits indexable). Hash is set to 0. ClassIndex is set to 19 or 18. These don't refer to any class!! See VMMaker SpurMemoryManager>>#sixtyFourBitLongsClassIndexPun and VMMaker SpurMemoryManager>>#thirtyTwoBitLongsClassIndexPun.

The purpose of this class is to create correctly these objects in a new Spur image.

'/Users/juanvuletich/Cuis-Smalltalk/Cuis-Smalltalk-Dev/Cuis6.0-5797.image'  asFileEntry readStreamDo: [ :fs | fs binary. cuisSpur64ImageFirstBytes := fs next: 1000 ].
'/Users/juanvuletich/Cuis-Smalltalk/Cuis-Smalltalk-Dev/Cuis6.0-5797-32.image'  asFileEntry readStreamDo: [ :fs | fs binary. cuisSpur32ImageFirstBytes := fs next: 1000 ].

(cuisSpur64ImageFirstBytes uint64At: 128+1+(8*6) bigEndian: false) hex. '16r 40  000000    09  000013' 
slots: 16r40=64
hash:0
format: 9 (64 bits indexable)
classIndex: 16r13=19 (VMMaker SpurMemoryManager>>#sixtyFourBitLongsClassIndexPun, not a class!!)
Siguen 64*8=512 bytes en cero. Despues viene el HiddenRoots, que es un objeto de mas de 255 slots!!
(cuisSpur64ImageFirstBytes uint64At: 128+1+(8*6)+8+512 bigEndian: false) hex.


(cuisSpur32ImageFirstBytes uint64At: 64+1+(8*6) bigEndian: false) hex. '16r 20  000000   0A  000012' 
slots: 16r20=32
hash=0
format: 0A (32 bits indexable)
classIndex: 16r12=18 (VMMaker SpurMemoryManager>>#thirtyTwoBitLongsClassIndexPun, not a class!!)
siguen 32*4=128 bytes en cero. Despues viene el HiddenRoots, que es un objeto de mas de 255 slots!!
(cuisSpur32ImageFirstBytes uint64At: 64+1+(8*6)+8+128 bigEndian: false) hex.!

!SpurPseudoArrayForeignObject commentStamp: 'jmv 5/20/2023 09:30:55' prior: 0!
The Spur Class Table and Class Table Page objects ar pretty special. They are very much like a regular Smalltalk Array. But they are is not visible from the Smalltalk side. So, they are not an instance of any class. ClassIndex is set to 16, witch doesn't refer to any class.
See VMMaker SpurMemoryManager>>#arrayClassIndexPun.

However, our representedObject is a regular Array, that holds the required information (oops) that are needed to create the pseudo-Array in the image being built.

The purpose of this class is to create correctly these objects in a new Spur image.!

!ImmediateForeignObject commentStamp: '<historical>' prior: 0!
ImmediateForeignObjects represent objects that don't need to be created in the heap, as all the information required is encoded in the Oop.!

!ImageBuilder methodsFor: 'initialization' stamp: 'jmv 7/14/2023 12:12:19'!
initialize
	isBigEndian := false. "default"
	classes := OrderedDictionary new.
	methods := IdentitySet new.
	baseAddressOfImage := self imageHeaderSize. 										"Base address. Start of Object Memory."
	nextFreeAddressOfImage := baseAddressOfImage.
	foreignObjectsMap := IdentityDictionary new.
	globalVariables  := OrderedCollection new.
	symbols := OrderedCollection new.! !

!ImageBuilder methodsFor: 'image building' stamp: 'jmv 11/2/2023 10:35:03'!
addBasicMethodsAndReferencedClasses

	"Classes referenced from methods below, but of which no methods are included."
	#(#ZeroDivide)
	do: [ :className | | classSpec classForeignObject |
		classSpec := ClassSpec for: (Smalltalk at: className) addClassesTo: classes.	
		classForeignObject := self addClassAndMetaclassSpec: classSpec classIndexOrNil: nil.
		self map: classSpec toForeignObject: classForeignObject ].

	"Basic methods. Will usually be needed by most images.
	MethodClass and superclasses also automatically included."
	{
		ProtoObject>>#initialize.
		ProtoObject>>#isNil.
		ProtoObject>>#notNil.
		ProtoObject>>#==.
		ProtoObject>>#~~.
		ProtoObject >> #identityHash.
		ProtoObject>>#noclash01_become:.

		UndefinedObject>>#isNil.
		UndefinedObject>>#notNil.

		Object>>#=.
		Object>>#~=.
		Object>>#hash.
		Object>>#isString.
		Object>>#noclash01_printOn:.
		Object>>#noclash01_doesNotUnderstand:.
		Object>>#noclash01_error:.
		Object>>#noclash01_warning:.
		Object>>#class.
		Object>>#at:.
		Object>>#at:put:.
		Object>>#basicAt:.
		Object>>#isMethodProperties.
		Object>>#isNumber.
		Object>>#isByteString.
		Object>>#isUnicodeString.
		Object>>#isSymbol.
		Object>>#isUnicodeCodePoint.
		Object>>#isBehavior.
		Object>>#isInterval.
		Object>>#value.
		Object>>#is:.
		Object>>#isInteger.
		Object>>#perform:.
		Object>>#basicSize.

		Boolean>>#|.
		Boolean>>#&.
		Boolean>>#not.
		True>>#|.
		True>>#&.
		True>>#not.
		False>>#|.
		False>>#&.
		False>>#not.

		Behavior>>#new.
		Behavior>>#basicNew.
		Behavior>>#new:.
		Behavior>>#basicNew:.
		Behavior>>#isMeta.
		Behavior >> #identityHash.
		Behavior >> #primitiveIdentityHash.
		ClassDescription>>#printNameOn:.
		ClassDescription>>#theNonMetaClass.
		Class>>#printNameOn:.
		Metaclass>>#isMeta.
		Metaclass>>#printNameOn:.
		Metaclass>>#theNonMetaClass.

		Magnitude>>#=.
		Magnitude>>#hash.
		Magnitude>>#min:.
		Magnitude>>#max:.
		Magnitude>>#between:and:.
		Number>>#=.
		Number>>#//.
		Number>>#isNumber.
		Number>>#asInteger.
		Number>>#negated.
		Number>>#is:.
		Number>>#printString.
		Number>>#isZero.
		Number>>#ceiling.
		Number>>#raisedToInteger:.
		Number>>#negative.
		Number>>#floor.
		Integer>>#=.
		Integer>>#//.
		Integer>>#is:.
		Integer>>#asInteger.
		Integer>>#isInteger.
		Integer>>#highBit.
		Integer>>#highBitOfMagnitude.
		Integer>>#floor.
		SmallInteger>>#=.
		SmallInteger>>#<.
		SmallInteger>>#<=.
		SmallInteger>>#>.
		SmallInteger>>#>=.
		SmallInteger>>#~=.
		SmallInteger>>#+.
		SmallInteger>>#-.
		SmallInteger>>#*.
		SmallInteger>>#//.
		SmallInteger>>#\\.
		SmallInteger>>#identityHash.
		SmallInteger>>#quo:.
		SmallInteger>>#bitAnd:.
		SmallInteger>>#bitOr:.
		SmallInteger>>#bitShift:.
		SmallInteger>>#printString.
		SmallInteger>>#decimalDigitLength.
		SmallInteger>>#even.
		SmallInteger>>#lowBit.
		SmallInteger>>#bitXor:.

		CompiledMethod>>#methodClass.
		CompiledMethod>>#numLiterals.
		CompiledMethod>>#header.
		CompiledMethod>>#objectAt:.
		CompiledMethod>>#literalAt:.
		CompiledMethod>>#selector.
		CompiledMethod>>#withPropertiesDo:ifSelector:.
		CompiledMethod>>#penultimateLiteral.
		AdditionalMethodState>>#isMethodProperties.
		AdditionalMethodState>>#selector.

		ContextPart>>#printOn:.
		ContextPart>>#methodClass.
		ContextPart>>#sender.
		ContextPart>>#selector.
		ContextPart>>#basicSize.
		MethodContext>>#printOn:.
		MethodContext>>#outerContext.
		MethodContext>>#method.
		MethodContext>>#receiver.
		MethodContext>>#tempAt:.
		BlockClosure>>#printOn:.
		BlockClosure>>#outerContext.
		BlockClosure>>#value.
		BlockClosure>>#value:.
		BlockClosure>>#value:value:.
		BlockClosure>>#value:value:value:.
		BlockClosure>>#valueWithPossibleArgument:.

		Message>>#selector.

		Collection class>>#ofSize:.
		Collection>>#species.
		SequenceableCollection class>>#streamContents:.
		SequenceableCollection class>>#streamContents:estimatedSize:.
		SequenceableCollection >> #do:.
		SequenceableCollection>>#copyReplaceFrom:to:with:.
		SequenceableCollection >> #,.
		SequenceableCollection>>#asByteArray.
		SequenceableCollection>>#replaceFrom:to:with:.
		SequenceableCollection>>#writeStream.
		SequenceableCollection>>#asStreamResult.
		SequenceableCollection>>#thatCanBeModified.
		SequenceableCollection>>#copyFrom:to:.
		ArrayedCollection class>>#withAll:.
		ArrayedCollection>>#size.
		Array>>#elementsExchangeIdentityWith:.
		ByteArray>>#replaceFrom:to:with:startingAt:.
		ByteArray>>#asString.
		ByteArray class>>#findFirstInBytes:inSet:startingAt:.

		CharacterSequence>>#printOn:.
		CharacterSequence>>#storeOn:.
		CharacterSequence>>#isString.
		CharacterSequence>>#string.
		String class>>#with:.
		String class>>#isAscii:.
		String>>#asUtf8Bytes.
		String>>#isAscii.
		String>>#size.
		String>>#at:put:.
		String>>#replaceFrom:to:with:startingAt:.
		String >> #,.
		String>>#appendToString:.
		String>>#copyReplaceFrom:to:with:.
		String>>#canHoldCodePoints.
		Symbol>>#noclash01_storeOn:.

		Character class>>#lf.
		Character class>>#newLineCharacter.
		Character class>>#space.
		Character class>>#digitValue:.
		Character class>>#noclash01_numericValue:.
		Character class>>#immediateNumericValue:.
		Character>>#identityHash.
		Character>>#is:.

		Stream>>#nextPutAll:.
		PositionableStream class>>#on:.
		PositionableStream>>#on:.
		PositionableStream>>#nextInto:.
		PositionableStream>>#originalContents.
		PositionableStream>>#position.
		PositionableStream>>#position:.
		PositionableStream>>#reset.
		PositionableStream>>#useBytes.
		WriteStream>>#on:.
		WriteStream>>#reset.
		WriteStream>>#nextPut:.
		WriteStream>>#nextPutAll:.
		WriteStream>>#isCompatibleWithContents:.
		WriteStream>>#contents.

		Association>>#value.

		SystemDictionary>>#quitPrimitive:.
		SystemDictionary>>#isSpur.
		SystemDictionary>>#compactClassesArray.
		SystemDictionary>>#specialObjectsArray.

		StdIOWriteStream class>>#stdout.
		StdIOWriteStream class>>#stdoutHandle.
		StdIOWriteStream class>>#stdioHandles.
		StdIOWriteStream>>#openOnHandle:name:.
		StdIOWriteStream>>#nextPutAll:.
		StdIOWriteStream>>#nextPut:.
		StdIOWriteStream>>#newLine.
		StdIOWriteStream>>#space.
		StdIOWriteStream>>#flush.
		StdIOWriteStream>>#noclash01_primWrite:from:startingAt:count:.
		StdIOWriteStream>>#primFlush:.


	} do: [ :compiledMethod |
		self addExistingMethod: compiledMethod ].! !

!ImageBuilder methodsFor: 'image building' stamp: 'jmv 7/17/2023 14:46:29'!
addFundamentalClasses
	"Some classes that are needed for any Cuis image:
	- nil, false and true are always present and need their class
	- all superclasses for VM known classes
	- all classes and superclasses for SpecialObjectsArray.
	Do not include those already added in #addVmKnownClasses.
	Add ForeignObjects both for class and metaclass to the list.
	But only add the one for the class to the foreignObjects dictionary, because they share the spec."
	"This first implementation uses the fact that these classes already exist in the image we are running.
	It would be better to build them from scratch, using alternative class protocol in ClassSpec,
	most likely by compiling a source code file."

	#(#UndefinedObject #False #True
		#ProtoObject #Object #Boolean #Magnitude #Number #Integer #Float
		#Collection #SequenceableCollection #CharacterSequence #Symbol #ArrayedCollection #LinkedList
		#Set #Dictionary #IdentityDictionary #SystemDictionary
		#LookupKey #Association
		#InstructionStream #ContextPart
		#ProcessorScheduler #Link #Process
		#Behavior #ClassDescription #Class #Metaclass
		#MethodDictionary
		#CompiledMethod #AdditionalMethodState #Pragma
		#Form #DisplayScreen
		#InputSensor #EventSensor #SharedQueue)
	do: [ :className | | classSpec classForeignObject |
		classSpec := ClassSpec for: (Smalltalk at: className) addClassesTo: classes.	
		classForeignObject := self addClassAndMetaclassSpec: classSpec classIndexOrNil: nil.
		self map: classSpec toForeignObject: classForeignObject ].! !

!ImageBuilder methodsFor: 'image building' stamp: 'jmv 9/18/2023 15:44:42'!
addFundamentalObjectsAndClasses
	"
	Objects and classes added here are absolutely required for building
	a healthy image that includes what the VM expects.
	"

	self addSpecial: nil hash: 1 nameOfClassToInstantiate: #UndefinedObject.
	self addSpecial: false hash: 2 nameOfClassToInstantiate: #False.
	self addSpecial: true hash: 3 nameOfClassToInstantiate: #True.
	
	self addSpurHiddenObjectsIfRequired.

	"SpecialObjects Array. Ivar specialObjectsArrayForeignObject holds its fo.
	Apparently the Spur VM saves the image with the SOA at this position,
	\after the ClassTable and its first two pages. Do the same here."
	self addSpecialObjectsArray.

	"Add classes known by the Spur VM"
	self addVmKnownClasses.
	"Add other classes absolutely required by the VM, and their superclasses."
	self addFundamentalClasses.

	"Create basic globals"
	"Smalltalk globals inspect"
	self addProcessor.
	self addDisplay.
	self addSensor.
	self addSmalltalk.
	self addUndeclared.

	"After adding classes, we can fill the contents of the SpecialObjects array"
	self fillSpecialObjectsArray.! !

!ImageBuilder methodsFor: 'image building' stamp: 'jmv 7/17/2023 12:52:26'!
addMethodDictionariesAndStuff
	"Fix Class hierarchies, build MethodDictionaries.
	Build the Smalltalk SystemDictionary."

	"Superclasses of method methodClass may not be mapped yet. Additionally, add all as subclasses of their superclass."
	classes do: [ :classSpec | | classForeignObject |
		classForeignObject := self findOrAddClassSpec: classSpec.
		classSpec superclass ifNotNil: [ :superclassSpec |
			(foreignObjectsMap at: superclassSpec) addSubclass: classForeignObject ]].

	"Now turn the subclasses OrderedCollections into a foreign object for an Array"
	classes do: [ :classSpec | | classForeignObject |
		classForeignObject :=  foreignObjectsMap at: classSpec.
		classForeignObject subclassesForeignObject:
			(self createForeignObjectFor: classForeignObject subclassForeignObjects asArray) ].

	"Build MethodDictionaries. No additional classes or methods will be allowed!!"
	classes do: [ :classSpec | | classForeignObject metaClassForeignObject |
		classForeignObject := foreignObjectsMap at: classSpec.
		self addMethodDictionaryFor: classForeignObject.
		metaClassForeignObject := classForeignObject metaclassForeignObject.
		self addMethodDictionaryFor: metaClassForeignObject ].

	"Now that we have all the required globals at ivar 'globalVariables', fill the contents of the Smalltalk object."
	self fillSmalltalkDictionary.! !

!ImageBuilder methodsFor: 'image building' stamp: 'jmv 7/17/2023 12:39:29'!
addSpecialObjectsArray
	"Create a SpecialObjectsArray for our new image. See #recreateSpecialObjectsArray.
	Add a ForeignObject for it to our foreign object list and dicitonary.
	See #fillSpecialObjectsArray."

	| specialObjectsArray |
	specialObjectsArray := Array new: 60.
	specialObjectsArrayForeignObject := self createForeignObjectFor: specialObjectsArray.! !

!ImageBuilder methodsFor: 'image building' stamp: 'jmv 7/17/2023 12:36:17'!
addSpurHiddenObjectsIfRequired
	"NOP here. Only for Spur."! !

!ImageBuilder methodsFor: 'image building' stamp: 'jmv 7/17/2023 16:19:13'!
addVmKnownClasses
"
Copied and adapted from the Spur implementation, that requires specific identityHash for these classe.
This implementation hasn't yet been tested!!
"
	"Some classes are known by the VM.
	- They have specific classIndex and identityHash
	- They are stored in the first page of the class table
	Add ForeignObjects both for class and metaclass to the list.
	But only add the one for the class to the foreignObjects dictionary, because they share the spec."
	"This first implementation uses the fact that these classes already exist in the image we are running.
	It would be better to build them from scratch, using alternative class protocol in ClassSpec,
	most likely by compiling a source code file."
self halt: 'No SmallFloats without Spur64!!'.
	#(
		#SmallInteger
		#Character
		#SmallFloat64
		#LargeNegativeInteger
		#LargePositiveInteger
		#BoxedFloat64
		#Message
		#MethodContext
		#BlockClosure
		#Semaphore
		#ByteArray
		#Array
		#String
		#Bitmap
		#Point)
	do: [ :className | | classSpec classForeignObject |
		classSpec := ClassSpec for: (Smalltalk at: className) addClassesTo: classes.	
		classForeignObject := self addClassAndMetaclassSpec: classSpec classIndexOrNil: nil.
		self map: classSpec toForeignObject: classForeignObject ].! !

!ImageBuilder methodsFor: 'image building' stamp: 'jmv 9/19/2023 13:55:27'!
createAndAddWarningMethodsForMissing
	"Create special methods to warn about improper execution of inherited methods when proper method was not included."

	| includedSelectorsByClass |
	includedSelectorsByClass := Dictionary new.
	methods do: [ :methodSpec |
		(includedSelectorsByClass at: {methodSpec classSpec. methodSpec isClassSide}
			ifAbsentPut: [ Set new ]) add: methodSpec selector ].

	classes do: [ :classSpec |
		{true. false} do: [ :classSideFlag |
			| includedSelectors existingClass |
			includedSelectors := includedSelectorsByClass at: {classSpec. classSideFlag} ifAbsent: [`Set new`].
			existingClass := 		Smalltalk at: classSpec name.
			classSideFlag ifTrue: [
				existingClass := existingClass class ].
			existingClass selectorsDo: [ :missingSelector |
				(includedSelectors includes: missingSelector) ifFalse: [
					(self includedDefinition: missingSelector in: classSpec isClassMethod: classSideFlag all: includedSelectorsByClass)
						ifNotNil: [ :inheritedFromString |
							self createWarningMethodFor: missingSelector in: classSpec isClassMethod: classSideFlag inheritedFrom: inheritedFromString ]
				]]]].! !

!ImageBuilder methodsFor: 'image building' stamp: 'jmv 9/19/2023 13:58:42'!
createWarningMethodFor: missingSelector in: classSpec isClassMethod: classSideFlag inheritedFrom: desc
	"Made by playing with a method with this source code:
		hack3
			self noclash01_warning: 'eso eso eso'.
	And then going with the debugger through
		MethodSpec for: self addClassesTo: IdentityDictionary new.	
	Did the same with #hack3: and #hack3:b:
	The only difference was in header. Took logic from
		#computeMethodHeaderForNumArgs:numTemps:numLits:primitive:"

	 | warningString fallbackMethodSpec |
	warningString := String streamContents: [ :strm |
		strm
			nextPutAll: '======== ';
			nextPutAll: classSpec name asString.
		classSideFlag ifTrue: [ strm nextPutAll: ' class' ].
		strm
			nextPutAll: '>>#';
			nextPutAll: missingSelector;
			nextPutAll: ' was not included, but it is inherited from ';
			nextPutAll: desc ].

"warningString print."
	fallbackMethodSpec := MethodSpec basicNew
		classSpec: classSpec
		isClassSide: classSideFlag
		selector: missingSelector
		header: 4 + (missingSelector numArgs bitShift: 24) + (missingSelector numArgs bitShift: 18)
		literals: {#warning:. warningString}
		pragmas: nil
		bytecodes: #[112 33 224 135 120].
	self addMethodSpec: fallbackMethodSpec.
	methods add: fallbackMethodSpec.
	! !

!ImageBuilder methodsFor: 'image building' stamp: 'jmv 7/17/2023 12:49:27'!
fillSpecialObjectsArray
	"Please see implementation for Spur.
	Also see #recreateSpecialObjectsArray.
	See reference implementations (MicroSqueak.pck.st and SystemTracing.pck.st)"

	"Maybe after implementing for all subclasses, move a refactored version up here."! !

!ImageBuilder methodsFor: 'image building' stamp: 'jmv 9/19/2023 13:58:34'!
includedDefinition: missingSelector in: classSpec isClassMethod: classSideFlag all: includedSelectorsByClass
	"missingSelector is defined for classSpec but wasn't included.
	Answer the inherited version that would be run instead, if any."

	classSpec superclass ifNotNil: [ :superclassSpec |
		(self includedDefinition: missingSelector start: superclassSpec isClassMethod: classSideFlag all: includedSelectorsByClass)
			ifNotNil: [ :includedDefinition |
				^classSideFlag
					ifTrue: [ includedDefinition name asString, ' class' ]
					ifFalse: [	 includedDefinition name asString ]]].

	"Class side methods can also be inherited from the instance side of class Class."
	classSideFlag ifTrue: [| classClassSpec |
		classClassSpec := classes at: #Class.
		(self includedDefinition: missingSelector start: classClassSpec isClassMethod: false all: includedSelectorsByClass)
			ifNotNil: [ :includedDefinition |
				^includedDefinition name asString ]].

	^nil! !

!ImageBuilder methodsFor: 'image building' stamp: 'jmv 9/19/2023 12:50:21'!
includedDefinition: missingSelector start: classSpec isClassMethod: classSideFlag all: includedSelectorsByClass
	"Find the closest included definition of missingSelector.
	Start search at classSpec and go upwards in the hierarchy,"

	classSpec withAllSuperclassesDo: [ :superclassSpec |
		includedSelectorsByClass
			at: { superclassSpec. classSideFlag }
			ifPresent: [ :includedSelectors |
				(includedSelectors includes: missingSelector) ifTrue: [
					^superclassSpec ]]].
	^nil! !

!ImageBuilder methodsFor: 'image building examples' stamp: 'jmv 9/19/2023 11:27:31'!
addExample03FloatMethods

	"Classes referenced from methods below, but of which no methods are included."
	#(#DomainError)
	do: [ :className | | classSpec classForeignObject |
		classSpec := ClassSpec for: (Smalltalk at: className) addClassesTo: classes.	
		classForeignObject := self addClassAndMetaclassSpec: classSpec classIndexOrNil: nil.
		self map: classSpec toForeignObject: classForeignObject ].

	{

		SmallInteger>>#asFloat.

		Float class>>#initialize.
		Float class>>#initClassCachedState.
		Float class>>#pi.
		Float class>>#fmin.
		Float class>>#denormalized.
		Float class>>#fminDenormalized.
		Float class>>#signPart:mantissaPart:exponentPart:.
		Float>>#printOn:base:.
		Float>>#isZero.
		Float>>#isNaN.
		Float>>#absPrintOn:base:.
		Float>>#absPrintExactlyOn:base:.
		Float>>#isInfinite.
		Float>>#significandAsInteger.
		Float>>#mantissaPart.
		Float>>#exponentPart.
		Float>>#signPart.
		Float>>#partValues:.
		Float>>#partValues:ifInfinite:ifNaN:.
		Float>>#partBits:.
		Float>>#basicAt:.
		Float>>#negated.
		Float>>#nextTowardsZero.
		Float>>#reciprocalLogBase2.

		SmallFloat64>>#=.
		SmallFloat64>>#<.
		SmallFloat64>>#<=.
		SmallFloat64>>#>.
		SmallFloat64>>#>=.
		SmallFloat64>>#~=.
		SmallFloat64>>#+.
		SmallFloat64>>#-.
		SmallFloat64>>#*.
		SmallFloat64>>#/.
		SmallFloat64>>#identityHash.
		SmallFloat64>>#exponent.
		SmallFloat64>>#ln.
		SmallFloat64>>#timesTwoPower:.
		SmallFloat64>>#truncated.

	} do: [ :compiledMethod |
		self addExistingMethod: compiledMethod ].! !

!ImageBuilder methodsFor: 'image building examples' stamp: 'jmv 9/14/2023 17:16:53'!
addExample04FileExperimentsMethods

	"Classes referenced from methods below, but of which no methods are included."
	#()
	do: [ :className | | classSpec classForeignObject |
		classSpec := ClassSpec for: (Smalltalk at: className) addClassesTo: classes.	
		classForeignObject := self addClassAndMetaclassSpec: classSpec classIndexOrNil: nil.
		self map: classSpec toForeignObject: classForeignObject ].

	{
		
		SystemDictionary >> #primImageName.
		SystemDictionary >> #primGetCurrentWorkingDirectoryUnix.
		SystemDictionary >> #platformName.
		SystemDictionary >> #getSystemAttribute:.
		SystemDictionary >> #listBuiltinModule:.
		SystemDictionary >> #listLoadedModule:.
		SystemDictionary >> #primVmFileName.
		SystemDictionary >> #primVmPath.

		BasicFileServices class >> #binaryContentsOf:.
		BasicFileServices class >> #primOpen:writable:.
		BasicFileServices class >> #primSize:.
		BasicFileServices class >> #primRead:into:startingAt:count:.
		BasicFileServices class >> #primClose:.

	} do: [ :compiledMethod |
		self addExistingMethod: compiledMethod ].! !

!ImageBuilder methodsFor: 'image building examples' stamp: 'jmv 9/19/2023 16:00:03'!
addExample05SymbolExperimentsMethods

	"Classes referenced from below, but of which no methods are included."
	#()
	do: [ :className | | classSpec classForeignObject |
		classSpec := ClassSpec for: (Smalltalk at: className) addClassesTo: classes.	
		classForeignObject := self addClassAndMetaclassSpec: classSpec classIndexOrNil: nil.
		self map: classSpec toForeignObject: classForeignObject ].

	"Classes included with ALL their methods."
	#( WeakArray SymbolSet)
	do: [ :className | | cls classSpec |
		cls := Smalltalk at: className.
		classSpec := ClassSpec for: cls addClassesTo: classes.	
		self findOrAddClassSpec: classSpec.
		cls class methodsDo: [ :compiledMethod |
			self addExistingMethod: compiledMethod ].
		cls methodsDo: [ :compiledMethod |
			self addExistingMethod: compiledMethod ]].

	{

		Behavior >> #noclash01_allInstances.

		Collection >> #inject:into:.
		Collection >> #count:.
		Collection >> #detect:ifNone:.
		SequenceableCollection >> #hashQuick:.
		ByteArray class >> #minSizeForHashQuick.
		ByteArray class >> #hashBytes:startingWith:.

		CharacterSequence>>#asSymbol.
		String class>>#is:equalTo:.
		String class>>#caseSensitiveOrder.
		String class>>#compare:with:collated:.
		String class>>#symbolClassToUse.
		String>>#at:.
		String>>#at:put:.
		String>>#isByteString.
		String >> #=.
		String >> #hash.
		Symbol class>>#intern:.
		Symbol class>>#basicFromString:.
		Symbol class >> #initialize.
		Symbol class >> #rehash.
		Symbol class >> #lookup:.
		Symbol>>#at:put:.
		Symbol>>#asSymbol.
		Symbol>>#string:.
		Symbol >> #isSymbol.
		Symbol >> #=.

	} do: [ :compiledMethod |
		self addExistingMethod: compiledMethod ].! !

!ImageBuilder methodsFor: 'image building examples' stamp: 'jmv 11/2/2023 10:31:20'!
addExample06LoadDynamicLibraryMethods

	"Classes referenced from below, but of which no methods are included."
	#(ReadWriteStream IdentitySet)
	do: [ :className | | classSpec classForeignObject |
		classSpec := ClassSpec for: (Smalltalk at: className) addClassesTo: classes.	
		classForeignObject := self addClassAndMetaclassSpec: classSpec classIndexOrNil: nil.
		self map: classSpec toForeignObject: classForeignObject ].

"ojo, incluir todos los implementors de los metodos en todas las clases incluidas, o al menos en la herencia..."
	"Classes included with ALL their methods."
	#( WeakArray SymbolSet DynamicCuisLibrary ClassSpec MethodSpec LiteralVariableSpec)
	do: [ :className | | cls classSpec |
		cls := Smalltalk at: className.
		classSpec := ClassSpec for: cls addClassesTo: classes.	
		self findOrAddClassSpec: classSpec.
		cls class methodsDo: [ :compiledMethod |
			self addExistingMethod: compiledMethod ].
		cls methodsDo: [ :compiledMethod |
(compiledMethod selector = #buildClassesAndMethods:) ifFalse: [ "HACKY!!!!!!!!!!!!"
			self addExistingMethod: compiledMethod
]
			]].

	{
		Object>>#as:.
		Object>>#yourself.
		Object>>#->.
		Object>>#copy.
		Object>>#shallowCopy.
		Object>>#postCopy.
		Object>>#readDataFrom:size:.
		Object>>#	instVarAt:put:.
		Object>>#comeFullyUpOnReload:.
		Object>>#basicAt:put:.

		Behavior >> #noclash01_allInstances.
		Behavior>>#isBehavior.
		Behavior>>#superclass.
		Behavior>>#typeOfClass.
		Behavior>>#typeOfClassSpur.
		Behavior>>#isVariable.
		Behavior>>#isVariableSpur.
		Behavior>>#isBytes.
		Behavior>>#isBits.
		Behavior>>#isImmediateClass.
		Behavior>>#instSpec.
		Behavior>>#instSize.
		Behavior>>#definesInstanceVariableNamed:.
		Behavior>>#instVarNames.
		Behavior>>#format.
		Behavior>>#superclass:methodDictionary:format:.
		Behavior>>#binding.
		Behavior>>#addSelectorSilently:withMethod:.
		Behavior>>#lookupSelector:.
		Behavior>>#includesSelector:.
		Behavior>>#methodDict.
		Behavior>>#compiledMethodAt:.
		Behavior>>#bindingOf:.
		Behavior>>#classBindingOf:.
		Behavior>>#someInstance.

		ClassDescription>>#instVarNames.
		ClassDescription>>#organization:.
		ClassDescription>>#setInstVarNames:.
		ClassDescription>>#superclass:methodDictionary:format:.

		Class>>#superclass:methodDictionary:format:.
		Class>>#addSubclass:.
		Class>>#setName:.
		Class>>#declareClassVariables:.
		Class>>#name.
		Class>>#classPool.
		Class>>#sharedPools.
		Class>>#binding.
		Class>>#bindingOf:.
		Class>>#localBindingOf:.
		Class>>#classVarBindingOf:.
		Class>>#setName:.

		Metaclass>>#new.
		Metaclass>>#bindingOf:.
		Metaclass>>#localBindingOf:.
		Metaclass>>#allClassInstVarNames.

		CompiledMethod class>>#createMethod:trailer:header:.
		CompiledMethod class>>#newMethod:header:.
		CompiledMethod>>#literalAt:put:.
		CompiledMethod>>#objectAt:put:.
		CompiledMethod>>#penultimateLiteral:.
		CompiledMethod>>#methodClass:.
		CompiledMethod>>#endPC.
		CompiledMethod>>#initialPC.
		CompiledMethod>>#flushCache.
		CompiledMethod>>#readDataFrom:size:.

		MethodContext>>#	instVarAt:put:.

		Collection class>>#newFrom:.
		Collection>>#=.
		Collection>>#is:.
		Collection>>#reject:.
		Collection>>#ifEmpty:.
		Collection>>#isSequenceable.
		Collection>>#asIdentitySet.
		Collection>>#addAll:.
		Collection>>#notEmpty.
		Collection >> #inject:into:.
		Collection >> #count:.
		Collection >> #detect:ifNone:.
		SequenceableCollection >> #hashQuick:.	
		SequenceableCollection>>#=.
		SequenceableCollection>>#thatCanBeModified.
		SequenceableCollection >> #select:.
		SequenceableCollection >>#collect:.
		SequenceableCollection>>#readStream.
		SequenceableCollection>>#includes:.
		SequenceableCollection>>#indexOf:.
		SequenceableCollection>>#indexOf:ifAbsent:.
		SequenceableCollection>>#indexOf:startingAt:ifAbsent:.
		SequenceableCollection>>#withIndexDo:.
		SequenceableCollection>>#hasEqualElements:.
		SequenceableCollection>>#isSequenceable.
		SequenceableCollection>>#copyWith:.
		ArrayedCollection>>#isEmpty.
		Array>>#replaceFrom:to:with:startingAt:.
		Array>>#asArray.
		Array>>#is:.
		ByteArray class >> #minSizeForHashQuick.
		ByteArray class >> #hashBytes:startingWith:.
		ByteArray>>#uint32At:bigEndian:.
		ByteArray>>#int32At:bigEndian:.
		ByteArray>>#uint16At:bigEndian:.
		ByteArray>>#float64At:bigEndian:.

		CharacterSequence >> #asSymbol.
		String class >> #is:equalTo:.
		String class >> #caseSensitiveOrder.
		String class >> #compare:with:collated:.
		String class >> #symbolClassToUse.
		String >> #at:.
		String >> #at:put:.
		String >> #isByteString.
		String >> #=.
		String >> #hash.
		String>>#is:.
		Symbol class >> #intern:.
		Symbol class >> #basicFromString:.
		Symbol class >> #initialize.
		Symbol class >> #rehash.
		Symbol class >> #lookup:.
		Symbol >> #at:put:.
		Symbol >> #asSymbol.
		Symbol >> #string:.
		Symbol >> #isSymbol.
		Symbol >> #=.
		Symbol>>#flushCache.

		Set class>>#new.
		Set class>>#new:.
		Set class>>#sizeFor:.
		Set>>#init:.
		Set>>#atNewIndex:put:.
		Set>>#fullCheck.
		Set>>#grow.
		Set>>#includes:.
		Set>>#findElementOrNil:.
		Set>>#scanFor:.
		Set>>#growSize.
		Set>>#noCheckAdd:.
		Set>>#postCopy.
		Set>>#size.
		Set>>#do:.
		Set>>#comeFullyUpOnReload:.
		IdentitySet>>#scanFor:.
		Dictionary>>#keysDo:.
		Dictionary>>#associationsDo:.
		Dictionary>>#postCopy.
		Dictionary>>#noCheckAdd:.
		Dictionary>>#at:ifPresent:.
		Dictionary>>#includes:.
		Dictionary>>#at:put:.
		Dictionary>>#scanFor:.
		Dictionary>>#includesKey:.
		Dictionary>>#at:.
		Dictionary>>#errorKeyNotFound:.
		Dictionary>>#at:ifAbsent:.
		Dictionary>>#scanFor:.
		Dictionary>>#keys.
		Dictionary>>#associationAt:ifAbsent:.
		Dictionary>>#bindingOf:.
		Dictionary>>#declare:from:.
		IdentityDictionary>>#scanFor:.

		MethodDictionary class>>#new.
		MethodDictionary class>>#new:.
		MethodDictionary>>#at:ifAbsent:.
		MethodDictionary>>#scanFor:.
		MethodDictionary>>#postCopy.
		MethodDictionary >>#keyAt:.
		MethodDictionary>>#associationAt:ifAbsent:.
		MethodDictionary>>#keysDo:.
		MethodDictionary>>#at:put:.
		MethodDictionary>>#grow. "become!!!!!!!!!!"
		MethodDictionary>>#associationsDo:.

		LookupKey class>>#key:.
		LookupKey>>#key:.
		LookupKey>>#key.
		Association class>>#key:value:.
		Association>>#value:.

		BasicFileServices class >> #binaryContentsOf:.
		BasicFileServices class >> #primOpen:writable:.
		BasicFileServices class >> #primSize:.
		BasicFileServices class >> #primRead:into:startingAt:count:.
		BasicFileServices class >> #primClose:.

		Stream>>#binary.
		Stream>>#nextString.
		Stream>>#nextUint32BigEndian:.
		Stream>>#nextInt32BigEndian:.
		Stream>>#nextUint16BigEndian:.
		Stream>>#nextFloat64BigEndian:.
		ReadStream>>#next.
		ReadStream>>#next:.
		ReadStream>>#next:into:startingAt:.
		WriteStream>>#on:.
		WriteStream>>#reset.
		
		DataStream class>>#on:.
		DataStream class>>#referenceTypes.
		DataStream class>>#noclash01_initCachedState.
		DataStream class>>#typeMap.
		DataStream >>#typeIDFor:.
		DataStream>>#setStream:.
		DataStream>>#next.
		DataStream>>#beginReference:.
		DataStream>>#maybeBeginReference:.
		DataStream>>#setCurrentReference:.
		DataStream>>#getCurrentReference.
		DataStream>>#readArray.
		DataStream>>#readNil.
		DataStream>>#readTrue.
		DataStream>>#readFalse.
		DataStream>>#readInteger.
		DataStream>>#readShortInst.
		DataStream>>#readByteArray.
		DataStream>>#readWordLike.
		DataStream>>#readFloat.
		DataStream>>#readReference.
		DataStream>>#vacantRef.
		DataStream>>#objectAt:.
		DataStream>>#readShortRef.
		DataStream>>#readInstance.
		DataStream>>#readSymbol.
		DataStream>>#readString.
		DataStream>>#readCharacter.
		ReferenceStream class>>#on:.
		ReferenceStream>>#setStream:.
		ReferenceStream>>#next.
		ReferenceStream>>#beginReference:.
		ReferenceStream>>#maybeBeginReference:.
		ReferenceStream>>#objectAt:.
		ReferenceStream>>#setCurrentReference:.
		ReferenceStream>>#getCurrentReference.
		ReferenceStream>>#noteCurrentReference:.
		ReferenceStream>>#isAReferenceType:.
		
		SystemDictionary>>#at:put:.
		SystemDictionary>>#wordSize.
		SystemDictionary >>#vmParameterAt:.
		SystemDictionary>>#maxIdentityHash.
		SystemDictionary>>#voidCogVMState.
		SystemDictionary>>#flushClassNameCache.
	
		ClassBuilder>>#newSubclassOf:type:instanceVariables:from:.
		ClassBuilder>>#computeFormat:instSize:forSuper:ccIndex:.
		ClassBuilder>>#computeFormat:fullInstancesSize:.
		ClassBuilder>>#format:variable:bitsUnitSize:pointers:weak:.
		ClassBuilder>>#privateNewSubclassOf:from:.
		ClassBuilder>>#privateNewSubclassOf:.
		ClassBuilder>>#initialize.

	} do: [ :compiledMethod |
		self addExistingMethod: compiledMethod ].! !

!ImageBuilder methodsFor: 'image building examples' stamp: 'jmv 11/2/2023 10:32:28'!
addExample07LoadDynamicLibraryMethods

	"Classes referenced from below, but of which no methods are included."
	#(ReadWriteStream IdentitySet
	)
	do: [ :className | | classSpec classForeignObject |
		classSpec := ClassSpec for: (Smalltalk at: className) addClassesTo: classes.	
		classForeignObject := self addClassAndMetaclassSpec: classSpec classIndexOrNil: nil.
		self map: classSpec toForeignObject: classForeignObject ].

"ojo, incluir todos los implementors de los metodos en todas las clases incluidas, o al menos en la herencia..."
	"Classes included with ALL their methods."
	#( WeakArray SymbolSet DynamicCuisLibrary ClassSpec MethodSpec LiteralVariableSpec
	)
	do: [ :className | | cls classSpec |
		cls := Smalltalk at: className.
		classSpec := ClassSpec for: cls addClassesTo: classes.	
		self findOrAddClassSpec: classSpec.
		cls class methodsDo: [ :compiledMethod |
			self addExistingMethod: compiledMethod ].
		cls methodsDo: [ :compiledMethod |
(compiledMethod selector = #buildClassesAndMethods:) ifFalse: [ "HACKY!!!!!!!!!!!!"
			self addExistingMethod: compiledMethod
]
			]].

	{
		Object>>#as:.
		Object>>#yourself.
		Object>>#->.
		Object>>#copy.
		Object>>#shallowCopy.
		Object>>#postCopy.
		Object>>#readDataFrom:size:.
		Object>>#	instVarAt:put:.
		Object>>#comeFullyUpOnReload:.
		Object>>#basicAt:put:.

		Behavior >> #noclash01_allInstances.
		Behavior>>#isBehavior.
		Behavior>>#superclass.
		Behavior>>#typeOfClass.
		Behavior>>#typeOfClassSpur.
		Behavior>>#isVariable.
		Behavior>>#isVariableSpur.
		Behavior>>#isBytes.
		Behavior>>#isBits.
		Behavior>>#isImmediateClass.
		Behavior>>#instSpec.
		Behavior>>#instSize.
		Behavior>>#definesInstanceVariableNamed:.
		Behavior>>#instVarNames.
		Behavior>>#format.
		Behavior>>#superclass:methodDictionary:format:.
		Behavior>>#binding.
		Behavior>>#addSelectorSilently:withMethod:.
		Behavior>>#lookupSelector:.
		Behavior>>#includesSelector:.
		Behavior>>#methodDict.
		Behavior>>#compiledMethodAt:.
		Behavior>>#bindingOf:.
		Behavior>>#classBindingOf:.
		Behavior>>#someInstance.

		ClassDescription>>#instVarNames.
		ClassDescription>>#organization:.
		ClassDescription>>#setInstVarNames:.
		ClassDescription>>#superclass:methodDictionary:format:.

		Class>>#superclass:methodDictionary:format:.
		Class>>#addSubclass:.
		Class>>#setName:.
		Class>>#declareClassVariables:.
		Class>>#name.
		Class>>#classPool.
		Class>>#sharedPools.
		Class>>#binding.
		Class>>#bindingOf:.
		Class>>#localBindingOf:.
		Class>>#classVarBindingOf:.
		Class>>#setName:.

		Metaclass>>#new.
		Metaclass>>#bindingOf:.
		Metaclass>>#localBindingOf:.
		Metaclass>>#allClassInstVarNames.

		CompiledMethod class>>#createMethod:trailer:header:.
		CompiledMethod class>>#newMethod:header:.
		CompiledMethod>>#literalAt:put:.
		CompiledMethod>>#objectAt:put:.
		CompiledMethod>>#penultimateLiteral:.
		CompiledMethod>>#methodClass:.
		CompiledMethod>>#endPC.
		CompiledMethod>>#initialPC.
		CompiledMethod>>#flushCache.
		CompiledMethod>>#readDataFrom:size:.

		MethodContext>>#	instVarAt:put:.

		Collection class>>#newFrom:.
		Collection>>#=.
		Collection>>#is:.
		Collection>>#reject:.
		Collection>>#ifEmpty:.
		Collection>>#isSequenceable.
		Collection>>#asIdentitySet.
		Collection>>#addAll:.
		Collection>>#notEmpty.
		Collection >> #inject:into:.
		Collection >> #count:.
		Collection >> #detect:ifNone:.
		SequenceableCollection >> #hashQuick:.	
		SequenceableCollection>>#=.
		SequenceableCollection>>#thatCanBeModified.
		SequenceableCollection >> #select:.
		SequenceableCollection >>#collect:.
		SequenceableCollection>>#readStream.
		SequenceableCollection>>#includes:.
		SequenceableCollection>>#indexOf:.
		SequenceableCollection>>#indexOf:ifAbsent:.
		SequenceableCollection>>#indexOf:startingAt:ifAbsent:.
		SequenceableCollection>>#withIndexDo:.
		SequenceableCollection>>#hasEqualElements:.
		SequenceableCollection>>#isSequenceable.
		SequenceableCollection>>#copyWith:.
		ArrayedCollection>>#isEmpty.
		Array>>#replaceFrom:to:with:startingAt:.
		Array>>#asArray.
		Array>>#is:.
		ByteArray class >> #minSizeForHashQuick.
		ByteArray class >> #hashBytes:startingWith:.
		ByteArray>>#uint32At:bigEndian:.
		ByteArray>>#int32At:bigEndian:.
		ByteArray>>#uint16At:bigEndian:.
		ByteArray>>#float64At:bigEndian:.

		CharacterSequence >> #asSymbol.
		String class >> #is:equalTo:.
		String class >> #caseSensitiveOrder.
		String class >> #compare:with:collated:.
		String class >> #symbolClassToUse.
		String >> #at:.
		String >> #at:put:.
		String >> #isByteString.
		String >> #=.
		String >> #hash.
		String>>#is:.
		Symbol class >> #intern:.
		Symbol class >> #basicFromString:.
		Symbol class >> #initialize.
		Symbol class >> #rehash.
		Symbol class >> #lookup:.
		Symbol >> #at:put:.
		Symbol >> #asSymbol.
		Symbol >> #string:.
		Symbol >> #isSymbol.
		Symbol >> #=.
		Symbol>>#flushCache.

		Set class>>#new.
		Set class>>#new:.
		Set class>>#sizeFor:.
		Set>>#init:.
		Set>>#atNewIndex:put:.
		Set>>#fullCheck.
		Set>>#grow.
		Set>>#includes:.
		Set>>#findElementOrNil:.
		Set>>#scanFor:.
		Set>>#growSize.
		Set>>#noCheckAdd:.
		Set>>#postCopy.
		Set>>#size.
		Set>>#do:.
		Set>>#comeFullyUpOnReload:.
		IdentitySet>>#scanFor:.
		Dictionary>>#keysDo:.
		Dictionary>>#associationsDo:.
		Dictionary>>#postCopy.
		Dictionary>>#noCheckAdd:.
		Dictionary>>#at:ifPresent:.
		Dictionary>>#includes:.
		Dictionary>>#at:put:.
		Dictionary>>#scanFor:.
		Dictionary>>#includesKey:.
		Dictionary>>#at:.
		Dictionary>>#errorKeyNotFound:.
		Dictionary>>#at:ifAbsent:.
		Dictionary>>#scanFor:.
		Dictionary>>#keys.
		Dictionary>>#associationAt:ifAbsent:.
		Dictionary>>#bindingOf:.	
		Dictionary>>#declare:from:.
		IdentityDictionary>>#scanFor:.

		MethodDictionary class>>#new.
		MethodDictionary class>>#new:.
		MethodDictionary>>#at:ifAbsent:.
		MethodDictionary>>#scanFor:.
		MethodDictionary>>#postCopy.
		MethodDictionary >>#keyAt:.
		MethodDictionary>>#associationAt:ifAbsent:.
		MethodDictionary>>#keysDo:.
		MethodDictionary>>#at:put:.
		MethodDictionary>>#grow. "become!!!!!!!!!!"
		MethodDictionary>>#associationsDo:.

		LookupKey class>>#key:.
		LookupKey>>#key:.
		LookupKey>>#key.
		Association class>>#key:value:.
		Association>>#value:.

		BasicFileServices class >> #binaryContentsOf:.
		BasicFileServices class >> #primOpen:writable:.
		BasicFileServices class >> #primSize:.
		BasicFileServices class >> #primRead:into:startingAt:count:.
		BasicFileServices class >> #primClose:.

		Stream>>#binary.
		Stream>>#nextString.
		Stream>>#nextUint32BigEndian:.
		Stream>>#nextInt32BigEndian:.
		Stream>>#nextUint16BigEndian:.
		Stream>>#nextFloat64BigEndian:.
		ReadStream>>#next.
		ReadStream>>#next:.
		ReadStream>>#next:into:startingAt:.
		WriteStream>>#on:.
		WriteStream>>#reset.
		
		DataStream class>>#on:.
		DataStream class>>#referenceTypes.
		DataStream class>>#noclash01_initCachedState.
		DataStream class>>#typeMap.
		DataStream >>#typeIDFor:.
		DataStream>>#setStream:.
		DataStream>>#next.
		DataStream>>#beginReference:.
		DataStream>>#maybeBeginReference:.
		DataStream>>#setCurrentReference:.
		DataStream>>#getCurrentReference.
		DataStream>>#readArray.
		DataStream>>#readNil.
		DataStream>>#readTrue.
		DataStream>>#readFalse.
		DataStream>>#readInteger.
		DataStream>>#readShortInst.
		DataStream>>#readByteArray.
		DataStream>>#readWordLike.
		DataStream>>#readFloat.
		DataStream>>#readReference.
		DataStream>>#vacantRef.
		DataStream>>#objectAt:.
		DataStream>>#readShortRef.
		DataStream>>#readInstance.
		DataStream>>#readSymbol.
		DataStream>>#readString.
		DataStream>>#readCharacter.
		ReferenceStream class>>#on:.
		ReferenceStream>>#setStream:.
		ReferenceStream>>#next.
		ReferenceStream>>#beginReference:.
		ReferenceStream>>#maybeBeginReference:.
		ReferenceStream>>#objectAt:.
		ReferenceStream>>#setCurrentReference:.
		ReferenceStream>>#getCurrentReference.
		ReferenceStream>>#noteCurrentReference:.
		ReferenceStream>>#isAReferenceType:.
		
		SystemDictionary>>#at:put:.
		SystemDictionary>>#wordSize.
		SystemDictionary >>#vmParameterAt:.
		SystemDictionary>>#maxIdentityHash.
		SystemDictionary>>#voidCogVMState.
		SystemDictionary>>#flushClassNameCache.
	
		ClassBuilder>>#newSubclassOf:type:instanceVariables:from:.
		ClassBuilder>>#computeFormat:instSize:forSuper:ccIndex:.
		ClassBuilder>>#computeFormat:fullInstancesSize:.
		ClassBuilder>>#format:variable:bitsUnitSize:pointers:weak:.
		ClassBuilder>>#privateNewSubclassOf:from:.
		ClassBuilder>>#privateNewSubclassOf:.
		ClassBuilder>>#initialize.

		BlockClosure>>#timeToRun.
		Time class>>#millisecondsToRun:.
		Time class>>#localMillisecondClock.
		Time class>>#primLocalMicrosecondClock.

	} do: [ :compiledMethod |
		self addExistingMethod: compiledMethod ].! !

!ImageBuilder methodsFor: 'image building examples' stamp: 'jmv 9/19/2023 12:28:03'!
buildExample01Stdout
	"
	Spur64ImageBuilder new
		buildExample01Stdout;
		saveImageTo: 'Example01Stdout.image' asFileEntry.
	"
	"
	Spur32ImageBuilder new
		buildExample01Stdout;
		saveImageTo: 'Example01Stdout32.image' asFileEntry.
	"

	| startUpMethodForeignObject |

	"Any image will include these. They are mandatory."
	self addFundamentalObjectsAndClasses.

	"Most images will need these."
	self addBasicMethodsAndReferencedClasses.

	"Set what this image will do when run."
	startUpMethod := SystemDictionary>>#bootstrappedExample01Stdout.
	startUpMethodForeignObject := self addExistingMethod: startUpMethod.
	self map: startUpMethod toForeignObject: startUpMethodForeignObject.

	"Create special methods to warn about improper execution of inherited methods when proper method was not included."
	self createAndAddWarningMethodsForMissing.

	"Add additionally stuff to the image. This is the same for any image."
	self addMethodDictionariesAndStuff.

	"Creating the start process requires CompiledMethods to be there"
	self addStartProcess.! !

!ImageBuilder methodsFor: 'image building examples' stamp: 'jmv 9/19/2023 12:28:17'!
buildExample02DoesNotUnderstand
	"
	Spur64ImageBuilder new
		buildExample02DoesNotUnderstand;
		saveImageTo: 'Example02DoesNotUnderstand.image' asFileEntry.
	"
	"
	Spur32ImageBuilder new
		buildExample02DoesNotUnderstand;
		saveImageTo: 'Example02DoesNotUnderstand32.image' asFileEntry.
	"

	| startUpMethodForeignObject |

	"Any image will include these. They are mandatory."
	self addFundamentalObjectsAndClasses.

	"Most images will need these."
	self addBasicMethodsAndReferencedClasses.

	"Set what this image will do when run."
	startUpMethod := SystemDictionary>>#bootstrappedExample02DoesNotUnderstand.
	startUpMethodForeignObject := self addExistingMethod: startUpMethod.
	self map: startUpMethod toForeignObject: startUpMethodForeignObject.

	"Create special methods to warn about improper execution of inherited methods when proper method was not included."
	self createAndAddWarningMethodsForMissing.

	"Add additionally stuff to the image. This is the same for any image."
	self addMethodDictionariesAndStuff.

	"Creating the start process requires CompiledMethods to be there"
	self addStartProcess.! !

!ImageBuilder methodsFor: 'image building examples' stamp: 'jmv 9/19/2023 12:28:22'!
buildExample03Float
	"
	Spur64ImageBuilder new
		buildExample03Float;
		saveImageTo: 'Example03Float.image' asFileEntry.
	"
	"
	Spur32ImageBuilder new
		buildExample03Float;
		saveImageTo: 'Example03Float32.image' asFileEntry.
	"

	| startUpMethodForeignObject |

	"Any image will include these. They are mandatory."
	self addFundamentalObjectsAndClasses.

	"Most images will need these."
	self addBasicMethodsAndReferencedClasses.

	"Set what this image will do when run."
	self addExample03FloatMethods.
	startUpMethod := SystemDictionary>>#bootstrappedExample03Float.
	startUpMethodForeignObject := self addExistingMethod: startUpMethod.
	self map: startUpMethod toForeignObject: startUpMethodForeignObject.

	"Create special methods to warn about improper execution of inherited methods when proper method was not included."
	self createAndAddWarningMethodsForMissing.

	"Add additionally stuff to the image. This is the same for any image."
	self addMethodDictionariesAndStuff.

	"Creating the start process requires CompiledMethods to be there"
	self addStartProcess.! !

!ImageBuilder methodsFor: 'image building examples' stamp: 'jmv 9/19/2023 12:28:26'!
buildExample04FileExperiments
	"
	Spur64ImageBuilder new
		buildExample04FileExperiments;
		saveImageTo: 'Example04FileExperiments.image' asFileEntry.
	"
	"
	Spur32ImageBuilder new
		buildExample04FileExperiments;
		saveImageTo: 'Example04FileExperiments32.image' asFileEntry.
	"

	| startUpMethodForeignObject |

	"Any image will include these. They are mandatory."
	self addFundamentalObjectsAndClasses.

	"Most images will need these."
	self addBasicMethodsAndReferencedClasses.

	"Set what this image will do when run."
	self addExample04FileExperimentsMethods.
	startUpMethod := SystemDictionary>>#bootstrappedExample04FileExperiments.
	startUpMethodForeignObject := self addExistingMethod: startUpMethod.
	self map: startUpMethod toForeignObject: startUpMethodForeignObject.

	"Create special methods to warn about improper execution of inherited methods when proper method was not included."
	self createAndAddWarningMethodsForMissing.

	"Add additionally stuff to the image. This is the same for any image."
	self addMethodDictionariesAndStuff.

	"Creating the start process requires CompiledMethods to be there"
	self addStartProcess.! !

!ImageBuilder methodsFor: 'image building examples' stamp: 'jmv 9/19/2023 12:28:30'!
buildExample05SymbolExperiments
	"
	Spur64ImageBuilder new
		buildExample05SymbolExperiments;
		saveImageTo: 'Example05SymbolExperiments.image' asFileEntry.
	"
	"
	Spur32ImageBuilder new
		buildExample05SymbolExperiments;
		saveImageTo: 'Example05SymbolExperiments32.image' asFileEntry.
	"

	| startUpMethodForeignObject |

	"Any image will include these. They are mandatory."
	self addFundamentalObjectsAndClasses.

	"Most images will need these."
	self addBasicMethodsAndReferencedClasses.

	"Set what this image will do when run."
	self addExample05SymbolExperimentsMethods.
	startUpMethod := SystemDictionary>>#bootstrappedExample05SymbolExperiments.
	startUpMethodForeignObject := self addExistingMethod: startUpMethod.
	self map: startUpMethod toForeignObject: startUpMethodForeignObject.

	"Create special methods to warn about improper execution of inherited methods when proper method was not included."
	self createAndAddWarningMethodsForMissing.

	"Add additionally stuff to the image. This is the same for any image."
	self addMethodDictionariesAndStuff.

	"Creating the start process requires CompiledMethods to be there"
	self addStartProcess.! !

!ImageBuilder methodsFor: 'image building examples' stamp: 'jmv 11/2/2023 10:39:48'!
buildExample06LoadDynamicLibrary
	"
	Spur64ImageBuilder new
		buildExample06LoadDynamicLibrary;
		saveImageTo: 'Example06LoadDynamicLibrary.image' asFileEntry.
	"
	"
	Spur32ImageBuilder new
		buildExample06LoadDynamicLibrary;
		saveImageTo: 'Example06LoadDynamicLibrary32.image' asFileEntry.
	"
	"
	Feature require: 'TestDynamicCuisLibraries'.
	""
	| sourceForLibrary lib |
	sourceForLibrary := NoclashABC_DclExample03.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.
	lib writeOnFile: 'DclExample03.dcl' asFileEntry
	"
	| startUpMethodForeignObject |

	"Any image will include these. They are mandatory."
	self addFundamentalObjectsAndClasses.

	"Most images will need these."
	self addBasicMethodsAndReferencedClasses.

	"Set what this image will do when run."
	self addExample06LoadDynamicLibraryMethods.
	startUpMethod := SystemDictionary>>#bootstrappedExample06LoadDynamicLibrary.
	startUpMethodForeignObject := self addExistingMethod: startUpMethod.
	self map: startUpMethod toForeignObject: startUpMethodForeignObject.

	"Create special methods to warn about improper execution of inherited methods when proper method was not included."
	self createAndAddWarningMethodsForMissing.

	"Add additionally stuff to the image. This is the same for any image."
	self addMethodDictionariesAndStuff.

	"Creating the start process requires CompiledMethods to be there"
	self addStartProcess.! !

!ImageBuilder methodsFor: 'image building examples' stamp: 'jmv 9/20/2023 16:08:50'!
buildExample07LoadDynamicLibrary
	"
	Spur64ImageBuilder new
		buildExample07LoadDynamicLibrary;
		saveImageTo: 'Example07LoadDynamicLibrary.image' asFileEntry.
	"
	"
	Spur32ImageBuilder new
		buildExample06LoadDynamicLibrary;
		saveImageTo: 'Example07LoadDynamicLibrary32.image' asFileEntry.
	"

	| startUpMethodForeignObject |

	"Any image will include these. They are mandatory."
	self addFundamentalObjectsAndClasses.

	"Most images will need these."
	self addBasicMethodsAndReferencedClasses.

	"Set what this image will do when run."
	self addExample07LoadDynamicLibraryMethods.
	startUpMethod := SystemDictionary>>#bootstrappedExample07LoadDynamicLibrary.
	startUpMethodForeignObject := self addExistingMethod: startUpMethod.
	self map: startUpMethod toForeignObject: startUpMethodForeignObject.

	"Create special methods to warn about improper execution of inherited methods when proper method was not included."
	self createAndAddWarningMethodsForMissing.

	"Add additionally stuff to the image. This is the same for any image."
	self addMethodDictionariesAndStuff.

	"Creating the start process requires CompiledMethods to be there"
	self addStartProcess.! !

!ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 8/2/2023 10:44:05'!
addExistingMethod: aCompiledMethod

	| methodSpec methodForeignObject |
	self deny: (methods includes: aCompiledMethod).
	methodSpec := MethodSpec for: aCompiledMethod addClassesTo: classes.	
	methodForeignObject := self addMethodSpec: methodSpec.
	methods add: methodSpec.
	^methodForeignObject! !

!ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 6/29/2023 16:56:41'!
addGlobalNamed: aSymbol value: anObject
	"The key aSymbol is added to the foreignObjectsMap.
	The value is not, as it should be referenced through the key, or through the association, as a global binding.
	The association is also not mapped, as any reference to globals is handled via a LiteralVariableSpec,
	that will query the new globals dictionary when needed.

	Not mapping the global value and the association means there's some risk of someone else mapping them,
	therefore adding them twice to the new image.
	We may change this decision as we know better (jmv, May 24, 2023).

	Answer a two element array holding the ForeignObject of the Association(binding)
		and the ForeignObject of anObject."

	| keyForeignObject valueForeignObject globalBindingAssociation globalBindingAssociationForeignObject |
	"Add the global name to the list, to the map, and to the 'symbols' ivar."
	keyForeignObject := self findOrAddLiteral: aSymbol.

	"Don't add global value to the map. Usually, nobody should refer to it directly, only by name/binding."
	valueForeignObject := self findOrAdd: anObject.

	"Don't add the Association to the map. Access should usually be from CompiledMethods, and the
	corresponding LiteralVariableSpec will hold the name, and should now khow to get to them,"	
	globalBindingAssociation := Association key: keyForeignObject value: valueForeignObject.
	globalBindingAssociationForeignObject := self createForeignObjectFor: globalBindingAssociation.
	globalVariables add: globalBindingAssociationForeignObject.

	^{ globalBindingAssociationForeignObject. valueForeignObject }! !

!ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 8/2/2023 09:59:25'!
addMethodSpec: aMethodSpec

	| methodForeignObject classOrMetaclassForeignObject |
	self findOrAddLiteral: aMethodSpec selector.
	methodForeignObject := self createForeignObjectFor: aMethodSpec.
	classOrMetaclassForeignObject := self findOrAddClassSpec: aMethodSpec classSpec.
	aMethodSpec isClassSide ifTrue: [
		classOrMetaclassForeignObject := classOrMetaclassForeignObject metaclassForeignObject ].
	methodForeignObject methodClassForeignObject: classOrMetaclassForeignObject.
	classOrMetaclassForeignObject addMethod: methodForeignObject.

	"Aca es donde hay que agregar a la imagen cosas como literales, pragmas, ver qué más."
	aMethodSpec literals do: [ :each | | lit |
		lit := each.
		(lit is: #LiteralVariableSpec)
			ifTrue: [
				"If we are to ever be able to resolve this name, the name itself must already
				be there, as a key in some #classPool or the Smalltalk dictionary!!"
				"self findOrAddLiteral: lit variableName"]
			ifFalse: [
				self findOrAddLiteral: lit]].

	^methodForeignObject! !

!ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 6/23/2023 17:59:36'!
addSpecial: anObject hash: identityHash nameOfClassToInstantiate: nameOfClassToInstantiate
	"For special objects nil true, false"

	| foreignObject foClass |
	foClass := self foreignObjectClassFor: anObject.
	foreignObject := self createForeignObjectFor: anObject hash: identityHash class: foClass.
	foreignObject nameOfClassToInstantiate: nameOfClassToInstantiate.
	self map: anObject toForeignObject: foreignObject.
	^foreignObject! !

!ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 7/12/2023 11:34:06'!
createForeignObjectFor: anObject
	"anObject will be included in the new image
	Create and answer an instance of aForeignObjectClass for it.
	For objects that are allocated in memory:
		- Allocate space in the new image for it
		- Add it to the list of foreign objects we'll use to serialize.
	For objects that won't be allocated in memory (they are immediate objects,
	encoded in pointing OOPs, like SmallInteger, etc):
		- Just create and answer a ForeignObject that can encode object in an OOP when needed."

	| foreignObject foClass possiblyConvertedObject |
	"We can't convert UnicodeString to String here. We need to have the original object as key in the foreignObjectsMap"
	self deny: anObject class == UnicodeString.
	possiblyConvertedObject := anObject.
	possiblyConvertedObject class == UnicodeSymbol ifTrue: [
		self assert: possiblyConvertedObject isAscii.
		self halt.
		possiblyConvertedObject := Symbol basicFromString: possiblyConvertedObject asString asByteString ].
	foClass := self foreignObjectClassFor: possiblyConvertedObject.
	foreignObject := foClass instancesAreForImmediateObjects
		ifTrue: [ foClass newFor: possiblyConvertedObject ]
		ifFalse: [ self createForeignObjectFor: possiblyConvertedObject hash: self newObjectHash class: foClass ].
	^foreignObject! !

!ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 6/23/2023 15:46:02'!
createForeignObjectFor: anObject hash: identityHash class: aForeignObjectClass
	"anObject will be included in the new image
	Create and answer an instance of aForeignObjectClass for it.
	For objects that are allocated in memory:
		- Allocate space in the new image for it
		- Add it to the list of foreign objects we'll use to serialize.
	For objects that won't be allocated in memory (they are immediate objects,
	encoded in pointing OOPs, like SmallInteger, etc):
		- Just create and answer a ForeignObject that can encode object in an OOP when needed."

	self subclassResponsibility! !

!ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 6/23/2023 15:37:44'!
findClassNamed: aSymbol
	"Answer the foreign object that represents the class that will be named as the argument."

	^foreignObjectsMap at: (classes at: aSymbol)! !

!ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 6/26/2023 17:16:35'!
findForeignObjectFor: anObjectToInclude
	"anObjectToInclude must already be mapped,
	except for immediates. We can create them on the fly."

	| foClass |

	foreignObjectsMap at: anObjectToInclude ifPresent: [ :found | ^found ].

	foClass := self foreignObjectClassFor: anObjectToInclude.
	foClass instancesAreForImmediateObjects
		ifTrue: [ ^foClass newFor: anObjectToInclude ].

	^self error: 'Object not allocated (No ForeignObject found for it)'.! !

!ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 7/6/2023 09:45:10'!
findGlobalBindingNamed: aSymbol
	"Answer the foreign object for the Association that holds global named as the argument.
	Answer nil if no such global"

	^smalltalkForeignObject representedObject "a SystemDictionary for the new image"
		array representedObject  								"its #array"
			detect: [ :associationForeignObject |
				associationForeignObject notNil and: [
					associationForeignObject representedObject key representedObject
						= aSymbol ] ]
			ifNone: nil! !

!ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 6/23/2023 15:50:24'!
findOrAdd: anObjectToInclude
	"anObjectToInclude will be included in the new image.
	Create and add a ForeignObject if needed.
	Add it to the foreignObjectsMap, so we use the same instance next time anObjectToInclude shows up."
	| fo |
"Para la serializacion. Ir validando lo que vaya apareciendo...
Por ejemplo, las refs a globales deberian entrar por nombre. Si aca empezamos a meter lo que aparezca, podriamos, por ejemplo, duplicar Display, Sensor, Processor, Smalltalk."
	^foreignObjectsMap at: anObjectToInclude ifAbsent: [
		fo := self createForeignObjectFor: anObjectToInclude.
		self map: anObjectToInclude toForeignObject: fo.
		fo ]! !

!ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 6/26/2023 15:39:49'!
findOrAddClassSpec: classSpec
	"Ensure argument is added, but don't add it twice"

	| classForeignObject |
	foreignObjectsMap at: classSpec ifPresent: [ :clsFO |
		^clsFO ].
	classForeignObject := self addClassAndMetaclassSpec: classSpec classIndexOrNil: nil.
	self map: classSpec toForeignObject: classForeignObject.
	^classForeignObject.! !

!ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 6/29/2023 17:04:36'!
findOrAddLiteral: aLiteral
	"aLiteral can be a String or Symbol. Also an Array of them.
	Additionally, aLiteral may be any object that answers true to #isLiteral
	This restriction could be lifted. It is in place so we control and understand what we are adding!!
	"
	"We could consider searching foreignObjectsMap for any object equal to aLiteral.
	That would avoid, for example, may instances of the empty String, or things like that."
	"Ensure argument is added, but don't add it twice.
	
	Add argument to the list.
	Also add it to map.
	Also add it to the 'symbols' instance variable. Wel'll use this to create a Symbol Table in the new image.
	(This should be quite similar to how we build the new Smalltalk SystemDictionary for the new image.)"


	| newForeignObject |
	self assert: aLiteral isLiteral.
	
	foreignObjectsMap at: aLiteral ifPresent: [ :foreignObject |
		^foreignObject ].

	newForeignObject := self createForeignObjectFor: aLiteral.
	newForeignObject class instancesAreForImmediateObjects ifTrue: [
		^ newForeignObject ].

	self map: aLiteral toForeignObject: newForeignObject.

	aLiteral isSymbol ifTrue: [
		symbols add: newForeignObject ].
	
	newForeignObject referencedObjectsDo: [ :each |
		self findOrAddLiteral: each ].

	^newForeignObject! !

!ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 6/23/2023 17:03:49'!
foreignObjectClassFor: anObject
	"Answer the correct ForeignObject subclass to represent anObject.
	Redefined in subclasses for some specific cases."

	^ForeignObject subclassFor: anObject.! !

!ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 6/23/2023 15:50:18'!
map: localObject toForeignObject: foreignObject
	"Any reference to localObject will be converted to a reference to the object to be materialized by foreignObject."

	(foreignObjectsMap includesKey: localObject) ifTrue: [
		self error: 'Object to map to ForeignObjects added twice!!' ].
	foreignObjectsMap at: localObject put: foreignObject! !

!ImageBuilder methodsFor: 'globals' stamp: 'jmv 7/14/2023 10:53:26'!
addDisplay
	"This assumes the shape of DisplayScreen and included ivars will not change..."

	| display foreignObjectsForBindingAndValue bitmap bitmapFO |
	display := DisplayScreen basicNew.
	display setExtent: 8@8 depth: 32.
	foreignObjectsForBindingAndValue := self addGlobalNamed: #Display value: display.
	displayForeignObject := foreignObjectsForBindingAndValue second.
	
	bitmap := display bits.
	bitmapFO := self createForeignObjectFor: bitmap.
	self map: bitmap toForeignObject: bitmapFO.! !

!ImageBuilder methodsFor: 'globals' stamp: 'jmv 9/18/2023 15:38:49'!
addProcessor
	"This assumes the shape of ProcessorScheduler, Process, and included ivars will not change...
	When bootstrapping from files, or from a separate hierarchy,
		see MicroSqueak - MicroSqueakImageBuilder >> #buildScheduler"

	| processor foreignObjectsForBindingAndValue |
	processor := ProcessorScheduler basicNew.
	foreignObjectsForBindingAndValue := self addGlobalNamed: #Processor value: processor.
	processorBindingForeignObject := foreignObjectsForBindingAndValue first.! !

!ImageBuilder methodsFor: 'globals' stamp: 'jmv 5/23/2023 14:47:59'!
addSensor
	"The actual instance will be created on startup.
	See EventSensor class >> #install
	Here we add the global, so the association is ready for that method to actually assign to the global."

	self addGlobalNamed: #Sensor value: nil.! !

!ImageBuilder methodsFor: 'globals' stamp: 'jmv 6/1/2023 15:03:14'!
addSmalltalk
	"This assumes the shape of SystemDictionary will not change..."

	| smalltalkObject foreignObjectsForBindingAndValue |
	smalltalkObject := SystemDictionary basicNew.
	foreignObjectsForBindingAndValue := self addGlobalNamed: #Smalltalk value: smalltalkObject.
	smalltalkForeignObject := foreignObjectsForBindingAndValue second.! !

!ImageBuilder methodsFor: 'globals' stamp: 'jmv 9/15/2023 11:06:26'!
addStartProcess
	"This assumes the shape of ProcessorScheduler, Process, and included ivars will not change...
	When bootstrapping from files, or from a separate hierarchy,
		see MicroSqueak - MicroSqueakImageBuilder >> #buildScheduler"

	| methodContext startProcess processor emptyArray |
	emptyArray := #().
	methodContext := MethodContext
		sender: nil
		receiver: smalltalkForeignObject
		method: startUpMethod
		arguments: emptyArray.

	"Set program counter. Quire like doing:
	methodContext pc: startUpMethod initialPC,
	(as already done during MethodContext creation above)
	but we need do it for the target image word size, not the running image word size!!"
	methodContext pc: (startUpMethod numLiterals + 1) * self wordSize + 1.

	startProcess := Process
		forContext: (self createForeignObjectFor: methodContext)
		priority: 40.
	startProcess name: 'Startup'.
	self map: emptyArray toForeignObject: (self createForeignObjectFor: emptyArray).
	self findOrAdd: startProcess name.
	
	processor := processorBindingForeignObject representedObject value representedObject.
	processor
		instVarNamed: 'quiescentProcessLists'
		put: (self createForeignObjectFor: ((1 to: 80) collect: [ :i | self createForeignObjectFor: LinkedList new ])).
	processor
		instVarNamed: 'activeProcess'
		put: (self createForeignObjectFor: startProcess).! !

!ImageBuilder methodsFor: 'globals' stamp: 'jmv 9/18/2023 15:44:08'!
addUndeclared
	"This assumes the shape of Dictionary will not change..."

	| undeclaredDictionary dictArray dictArrayForeignObject |
	undeclaredDictionary := Dictionary new.
	"Now replace the array inside with a ForeignObject for it"
	dictArray := undeclaredDictionary array.
	dictArrayForeignObject := self createForeignObjectFor: dictArray.
	undeclaredDictionary instVarNamed: 'array' put: dictArrayForeignObject.
	self addGlobalNamed: #Undeclared value: undeclaredDictionary.! !

!ImageBuilder methodsFor: 'smalltalk dictionary' stamp: 'jmv 6/26/2023 18:01:46'!
fillSmalltalkDictionary
	"Create thre contents of the Smalltalk SystemDictionary for the new image.
	Do it here, by 'remote control'. Assumes the structure of SystemDictionary is same as in this image,
	reflected in this code."

	| smalltalkObject smalltalkSize smalltalkArray smalltalkArrayForeignObject keyForeignObject classForeignObject |

	"Fetch the newly created, empty Smalltalk remote object."
	smalltalkObject := smalltalkForeignObject representedObject.
	self assert: smalltalkObject == (globalVariables detect: [ :assocFO |
		assocFO representedObject key representedObject == #Smalltalk ])
			representedObject value representedObject.

	"Create large enough."
	smalltalkSize := classes size + globalVariables size.
	smalltalkArray := Array new: smalltalkSize * 2.

	"Fill the 'array' ivar, using the identityHash from the foreignObjects of the keys, as they will be
	assigned to them on image serialization. Do it both for classes and other globals."
	classes do: [ :classSpec |
		keyForeignObject := foreignObjectsMap at: classSpec name.
		classForeignObject := foreignObjectsMap at: classSpec.
		self systemDictionaryArray: smalltalkArray at: keyForeignObject put: classForeignObject  ].
	globalVariables do: [ :assocForeignObject |
		self systemDictionaryArray: smalltalkArray add: assocForeignObject  ].

	"Done. Give them to it."
	smalltalkObject instVarNamed: 'tally' put: smalltalkSize.
	smalltalkArrayForeignObject := self createForeignObjectFor: smalltalkArray.
	smalltalkObject instVarNamed: 'array' put: smalltalkArrayForeignObject.! !

!ImageBuilder methodsFor: 'smalltalk dictionary' stamp: 'jmv 5/24/2023 09:44:06'!
systemDictionaryArray: array add: anAssociationForeignObject
	"Essentially copied from Dictionary >> #add:
	Assumes SystemDictionary will be pretty much the same in the generated image.
	Raise an error if same key added twice: this is specific for adding globals to a new image,
	and we'd better know what we are doing."

	| keyForeignObject index |
	keyForeignObject := anAssociationForeignObject representedObject key.
	index := self systemDictionaryArray: array scanFor: keyForeignObject.
	(array at: index) ifNotNil: [ self error: 'Duplicated global!!' ].
	array at: index put: anAssociationForeignObject.! !

!ImageBuilder methodsFor: 'smalltalk dictionary' stamp: 'jmv 6/23/2023 15:15:45'!
systemDictionaryArray: array at: keyForeignObject put: classForeignObject
	"Essentially copied from Dictionary >> #at:put:
	Assumes SystemDictionary will be pretty much the same in the generated image.
	Raise an error if same key added twice: this is specific for adding globals to a new image,
	and we'd better know what we are doing."

	| index globalBindingAssociation globalBindingAssociationForeignObject |
	index := self systemDictionaryArray: array scanFor: keyForeignObject.
	(array at: index) ifNotNil: [ self error: 'Duplicated global!!' ].
	globalBindingAssociation := Association key: keyForeignObject value: classForeignObject.
	globalBindingAssociationForeignObject := self createForeignObjectFor: globalBindingAssociation.
	array at: index put: globalBindingAssociationForeignObject.! !

!ImageBuilder methodsFor: 'smalltalk dictionary' stamp: 'jmv 7/6/2023 13:10:36'!
systemDictionaryArray: array scanFor: keyForeignObject
	"Essentially copied from IdentityDictionary >> #scanFor:
	Assumes SystemDictionary will be pretty much the same in the generated image."

	| finish scale start element |
	finish := array size.
	scale := finish // (self maxIdentityHash + 1).
	scale = 0 ifTrue: [scale := 1].
	start := (keyForeignObject foreignIdentityHashFor: self) * scale \\ finish + 1.

	"Search from (hash mod size) to the end."
	start to: finish do: [ :index |
		((element := array at: index) == nil or: [element representedObject key == keyForeignObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do: [ :index |
		((element := array at: index) == nil or: [element representedObject key == keyForeignObject])
			ifTrue: [^ index ]].

	self error: 'There is no free space in the newly created Smalltalk dictionary!!'.! !

!ImageBuilder methodsFor: 'services' stamp: 'jmv 5/15/2023 12:10:14'!
newObjectHash
	^self subclassResponsibility! !

!ImageBuilder methodsFor: 'services' stamp: 'jmv 8/17/2023 15:42:14'!
on: aStream nexWordPut: aNumber
	"Write #wordSize bytes. Use 'isBigEndian' endianess."
	self subclassResponsibility! !

!ImageBuilder methodsFor: 'services' stamp: 'jmv 8/17/2023 15:53:45'!
on: aStream nextUint16Put: aNumber
	"Write 2 bytes. Use 'isBigEndian' endianess."

	aStream nextUint16Put: aNumber bigEndian: isBigEndian.! !

!ImageBuilder methodsFor: 'services' stamp: 'jmv 8/17/2023 15:53:51'!
on: aStream nextUint32Put: aNumber
	"Write 4 bytes. Use 'isBigEndian' endianess."

	aStream nextUint32Put: aNumber bigEndian: isBigEndian.! !

!ImageBuilder methodsFor: 'services' stamp: 'jmv 8/17/2023 15:53:57'!
on: aStream nextUint64Put: aNumber
	"Write 8 bytes. Use 'isBigEndian' endianess."

	aStream nextUint64Put: aNumber bigEndian: isBigEndian.! !

!ImageBuilder methodsFor: 'format constants' stamp: 'jmv 5/10/2023 12:04:59'!
imageFormat
	"Answer the number that identifies this image format"
	self subclassResponsibility ! !

!ImageBuilder methodsFor: 'format constants' stamp: 'jmv 5/10/2023 12:24:06'!
imageHeaderSize
	"In Bytes"
	self subclassResponsibility ! !

!ImageBuilder methodsFor: 'format constants' stamp: 'jmv 5/23/2023 16:29:37'!
maxIdentityHash
	"Answer the maximum identityHash value supported by the VM."

	self subclassResponsibility ! !

!ImageBuilder methodsFor: 'format constants' stamp: 'jmv 5/10/2023 12:26:29'!
wordSize
	"Also size of an oop.
	In Bytes"
	self subclassResponsibility ! !

!ImageBuilder methodsFor: 'image file saving' stamp: 'jmv 8/22/2023 12:50:58'!
writeImageFileHeaderOn: aStream byteSize: imageByteSizeIncludingHeader
	"Write an appropriate image file header.
	Intended to be called after saving the heap to the file, and setting #position: to zero."
"
readImageFromFile:HeapSize:StartingAt:
"
	| specialObjectsOop savedWindowSize imageHeaderFlags extraVMMemory 
	desiredNumStackPages cogCodeSizeKB desiredEdenBytes maxExtSemTabSize the2ndUnknownShort_maxLiteralCountForCompile firstSegmentByteSize bytesLeftInOldSpace |

	false ifTrue: [ lastHash := lastHash ].
	specialObjectsOop := specialObjectsArrayForeignObject foreignOopFor: self.
	savedWindowSize := 640<<16 + 480.
#setImageHeaderFlagsFrom:.
	imageHeaderFlags := 130. 								"See #setImageHeaderFlagsFrom: and #getImageHeaderFlags and #imageHeaderFlags"
	extraVMMemory := 0. 								"See #extraVMMemory and #extraVMMemory:"

	desiredNumStackPages := 0. 								"Apparently 8 for v3, 0 for Spur means default"
	cogCodeSizeKB := 0.								"Apparently 0 for Spur"
	desiredEdenBytes := 0.								"Apparently 0 for Spur, meaning default? The VM saves Cuis with 16777216 ..."
	maxExtSemTabSize := 256 * 0.								"Apparently 0 for Spur? The VM saves Cuis with 0 ..."
	the2ndUnknownShort_maxLiteralCountForCompile := 0.
	firstSegmentByteSize := imageByteSizeIncludingHeader - self imageHeaderSize.
	bytesLeftInOldSpace := 0. 								"???? freeOldSpaceInImage"

	self on: aStream nextUint32Put: self imageFormat.
	self on: aStream nextUint32Put: self imageHeaderSize.
	self on: aStream nexWordPut: imageByteSizeIncludingHeader - self imageHeaderSize. 																"length of heap in file"
	self on: aStream nexWordPut: baseAddressOfImage.																"object memory base address of image"
	self on: aStream nexWordPut: specialObjectsOop.
	self on: aStream nexWordPut: lastHash.
	self on: aStream nexWordPut: savedWindowSize.
	self on: aStream nexWordPut: imageHeaderFlags.
	self on: aStream nextUint32Put: extraVMMemory.

	"Only for Cog"
	aStream nextUint16Put: desiredNumStackPages bigEndian: isBigEndian.
	aStream nextUint16Put: cogCodeSizeKB bigEndian: isBigEndian.

	"40 o 64 bytes so far"

	self on: aStream nextUint32Put: desiredEdenBytes.
	aStream nextUint16Put: maxExtSemTabSize bigEndian: isBigEndian.
	aStream nextUint16Put: the2ndUnknownShort_maxLiteralCountForCompile bigEndian: isBigEndian.

	"48 o 72 bytes so far"

	"Only for Spur????"
	self on: aStream nexWordPut: firstSegmentByteSize.
	self on: aStream nexWordPut: bytesLeftInOldSpace.! !

!SpurImageBuilder methodsFor: 'initialization' stamp: 'jmv 5/15/2023 16:25:30'!
initialize
	super initialize.
	lastHash := 1.
	lastClassIndex := 1023. "First regular class will have index 1024, and will be stored in the second page of the class table."! !

!SpurImageBuilder methodsFor: 'image building' stamp: 'jmv 7/17/2023 12:36:45'!
addSpurHiddenObjectsIfRequired

	"Create and add a SpurFreeListsForeignObject, to properly allocate the FreeLists object needed in Spur images."
	self addFreeLists.

	"Class Table and Hidden Roots. First page of the Class Table."
	self addClassTable.! !

!SpurImageBuilder methodsFor: 'image building' stamp: 'jmv 8/14/2023 16:48:52'!
addVmKnownClasses
	"Some classes are known by the VM.
	- They have specific classIndex and identityHash
	- They are stored in the first page of the class table
	Add ForeignObjects both for class and metaclass to the list.
	But only add the one for the class to the foreignObjects dictionary, because they share the spec."
	"This first implementation uses the fact that these classes already exist in the image we are running.
	It would be better to build them from scratch, using alternative class protocol in ClassSpec,
	most likely by compiling a source code file."

	"Note: SmallFloat64 will never be instantiated in Spur32!!"

	#(
		#(#SmallInteger 1)
		#(#Character 2)
		#(#SmallFloat64 4)
		#(#LargeNegativeInteger 32)
		#(#LargePositiveInteger 33)
		#(#BoxedFloat64 34)
		#(#Message 35)
		#(#MethodContext 36)
		#(#BlockClosure 37)
		#(#Semaphore 48)
		#(#ByteArray 50)
		#(#Array 51)
		#(#String 52)
		#(#Bitmap 53)
		#(#Point 54)
	) do: [ :pair | | className specialHash classSpec classForeignObject |
		className := pair first.
		specialHash := pair second.
		classSpec := ClassSpec for: (Smalltalk at: className) addClassesTo: classes.
		classForeignObject := self addClassAndMetaclassSpec: classSpec classIndexOrNil: specialHash.
		self map: classSpec toForeignObject: classForeignObject ].! !

!SpurImageBuilder methodsFor: 'image building' stamp: 'jmv 7/17/2023 12:48:25'!
fillSpecialObjectsArray
	"Create a SpecialObjectsArray for our new image. See #recreateSpecialObjectsArray.
	Add a ForeignObject for it to our foreign object list and dicitonary.
	Assume the required classes where already added, and their specs are available at ivar 'classes'
	If we were to do a full bootstrap from sources, it is worh considering that the new image will most likely
	have a #recreateSpecialObjectsArray or similar method.
	It would be good to find a way to guarantee that the contents of this array in the bootstrapped image is consistent with it."
"This is done right now for Spur. V3 needs tweaks, for instance:
25: Character table
29: compactClassesArray.
32, 33, 34. Check in V3. Are the instance prototypes still needed in V3?
36 ?
56?"
	"See MicroSqueak - MicroSqueakImageBuilder >> #buildSpecialObjectsArray
	Many entries can be made nil if uninteresting, at least for older VMs."

"Ya mismo. nil, false, true, clases, symbols, arrays de ellos"
	| specialObjectsArray specialSelectors primitiveErrors |
	specialObjectsArray := specialObjectsArrayForeignObject representedObject.
	"Nil false and true get used throughout the interpreter"
	specialObjectsArray at: 1 put: nil.
	specialObjectsArray at: 2 put: false.
	specialObjectsArray at: 3 put: true.
	"This association holds the active process (a ProcessScheduler)"
	"Foreign object for the Association in the new Globals dictionary holding Processor"
	specialObjectsArray at: 4 put: processorBindingForeignObject.
	self assert: processorBindingForeignObject == (globalVariables detect: [ :assocFO |
		assocFO representedObject key representedObject == #Processor ]).
	"Numerous classes below used for type checking and instantiation"
	specialObjectsArray at: 5 put: (classes at: #Bitmap).
	specialObjectsArray at: 6 put: (classes at: #SmallInteger).
	specialObjectsArray at: 7 put: (classes at: #String).
	specialObjectsArray at: 8 put: (classes at: #Array).
	"ForeignObjct for the new Smalltalk global dictionary."
	specialObjectsArray at: 9 put: smalltalkForeignObject.
	self assert: smalltalkForeignObject == ((globalVariables detect: [ :assocFO |
		assocFO representedObject key representedObject == #Smalltalk ]) representedObject value).
	specialObjectsArray at: 10 put: (classes at: #BoxedFloat64).
	specialObjectsArray at: 11 put: (classes at: #MethodContext).
	specialObjectsArray at: 12 put: nil. "was BlockContext."
	specialObjectsArray at: 13 put: (classes at: #Point).
	specialObjectsArray at: 14 put: (classes at: #LargePositiveInteger).
	"ForeignObjct for the new Display global Displaycreen."
	specialObjectsArray at: 15 put: displayForeignObject.
	self assert: displayForeignObject == ((globalVariables detect: [ :assocFO |
		assocFO representedObject key representedObject == #Display ]) representedObject value).
	specialObjectsArray at: 16 put: (classes at: #Message).
	specialObjectsArray at: 17 put: (classes at: #CompiledMethod).
	specialObjectsArray at: 18 put: (self createForeignObjectFor: Semaphore new). "low space Semaphore"
	specialObjectsArray at: 19 put: (classes at: #Semaphore).
	specialObjectsArray at: 20 put: (classes at: #Character).
	specialObjectsArray at: 21 put: (self findOrAddLiteral: #doesNotUnderstand:).
	specialObjectsArray at: 22 put: (self findOrAddLiteral: #cannotReturn:).
	specialObjectsArray at: 23 put: nil. "This is the process signalling low space."
	"An array of the 32 selectors that are compiled as special bytecodes,
	 paired alternately with the number of arguments each takes."
	specialSelectors := #(	#+ 1 #- 1 #< 1 #> 1 #<= 1 #>= 1 #= 1 #~= 1
							#* 1 #/ 1 #\\ 1 #@ 1 #bitShift: 1 #// 1 #bitAnd: 1 #bitOr: 1
							#at: 1 #at:put: 2 #size 0 #next 0 #nextPut: 1 #atEnd 0 #== 1 #class 0
							#blockCopyNOWUNUSED: 1 #value 0 #value: 1 #do: 1 #new 0 #new: 1 #x 0 #y 0 ).
	specialObjectsArray at: 24 put: (self findOrAddLiteral: specialSelectors).
	specialObjectsArray at: 25 put: nil. "was Character table"
	specialObjectsArray at: 26 put: (self findOrAddLiteral: #mustBeBoolean).
	specialObjectsArray at: 27 put: (classes at: #ByteArray).
	specialObjectsArray at: 28 put: (classes at: #Process).
	"An array of up to 31 classes whose instances will have compact headers; an empty array in Spur"
	specialObjectsArray at: 29 put: nil.  "was compactClassesArray."
	specialObjectsArray at: 30 put: (self createForeignObjectFor: Semaphore new). "delay Semaphore"
	specialObjectsArray at: 31 put: (self createForeignObjectFor: Semaphore new). "user interrupt Semaphore"
	specialObjectsArray at: 32 put: nil. "was the prototype Float"
	specialObjectsArray at: 33 put: nil. "was the prototype 4-byte LargePositiveInteger"
	specialObjectsArray at: 34 put: nil. "was the prototype Point"
	specialObjectsArray at: 35 put: (self findOrAddLiteral: #cannotInterpret:).
	specialObjectsArray at: 36 put: nil. "was the prototype MethodContext"
	specialObjectsArray at: 37 put: (classes at: #BlockClosure).
	specialObjectsArray at: 38 put: nil. "was the prototype BlockContext"

	"array of objects referred to by external code"
	specialObjectsArray at: 39 put: (self createForeignObjectFor:(Array new: 20)).
	specialObjectsArray at: 40 put: nil. "Reserved for Mutex in Cog VMs"
	specialObjectsArray at: 41 put: (self createForeignObjectFor: LinkedList new). "Reserved for a LinkedList instance for overlapped calls in CogMT"
	specialObjectsArray at: 42 put: (self createForeignObjectFor: Semaphore new). "finalization Semaphore"
	specialObjectsArray at: 43 put: (classes at: #LargeNegativeInteger).
	"External objects for callout."
	specialObjectsArray at: 44 put: nil.
	specialObjectsArray at: 45 put: nil.
	specialObjectsArray at: 46 put: nil.
	specialObjectsArray at: 47 put: nil.
	specialObjectsArray at: 48 put: nil.
	specialObjectsArray at: 49 put: (self findOrAddLiteral: #aboutToReturn:through:).
	specialObjectsArray at: 50 put: (self findOrAddLiteral: #run:with:in:).
	"51 reserved for immutability message"
	specialObjectsArray at: 51 put: (self findOrAddLiteral: #attemptToAssign:withIndex:).
	primitiveErrors := #(nil "nil => generic error" #'bad receiver'
							#'bad argument' #'bad index'
							#'bad number of arguments'
							#'inappropriate operation'  #'unsupported operation'
							#'no modification' #'insufficient object memory'
							#'insufficient C memory' #'not found' #'bad method'
							#'internal error in named primitive machinery'
							#'object may move' #'resource limit exceeded'
							#'object is pinned' #'primitive write beyond end of object').
	specialObjectsArray at: 52 put: (self findOrAddLiteral: primitiveErrors).
	"53 to 55 are for Alien"
	specialObjectsArray at: 53 put: nil.
	specialObjectsArray at: 54 put: (self findOrAddLiteral: #invokeCallbackContext:). "use invokeCallback:stack:registers:jmpbuf: for old Alien callbacks."
	specialObjectsArray at: 55 put: nil.

	"Weak reference finalization"
	specialObjectsArray at: 56 put: nil.

	"reserved for foreign callback process"
	specialObjectsArray at: 57 put: nil.

	specialObjectsArray at: 58 put: (self findOrAddLiteral: #unusedBytecode).
	"59 reserved for Sista counter tripped message"
	specialObjectsArray at: 59 put: (self findOrAddLiteral: #conditionalBranchCounterTrippedOn:).
	"60 reserved for Sista class trap message"
	specialObjectsArray at: 60 put: (self findOrAddLiteral: #classTrapFor:).! !

!SpurImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 8/14/2023 16:37:15'!
addClassAndMetaclassSpec: aClassSpec classIndexOrNil: classIndexOrNil
	"Add ForeignObjects both for class and metaclass to the list.
	But only add the one for the class to the foreignObjects dictionary, because they share the spec.
	classIndexOrNil if specified is used for the class."

	| classForeignObject metaclassForeignObject |
	classForeignObject := self addClassFor: aClassSpec classIndexOrNil: classIndexOrNil.
	metaclassForeignObject := self addMetaclassFor: aClassSpec.
	classForeignObject metaclassForeignObject: metaclassForeignObject.
	metaclassForeignObject thisClassForeignObject: classForeignObject.
"MethodDictionaries will only be added after adding all methods, as they need to be created large enough,
meaning we don't yet know the size they will have, and the memory space they will use.
Perhaps create the object but don't add it to the list yet... Or devise some way to hold the method specs and fo until we start serializing.
WARNING: this means once we start serializing we can no longer add methods.
The #addMethod: or whatever protocol must check that we haven't given oop and size to the method dict fo, or else..."
	"
	classForeignObject methodDictForeignObject: self addForeignObjectForMethodDictionary.
	metaclassForeignObject methodDictForeignObject: self addForeignObjectForMethodDictionary.
	"
	^classForeignObject
! !

!SpurImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 8/14/2023 16:37:20'!
addClassFor: aClassSpec classIndexOrNil: classIndexOrNil

	| classForeignObject classPool classPoolForeignObject classPoolArray classPoolArrayForeignObject |
	classForeignObject := self addClassSpec: aClassSpec class: ClassForeignObject classIndexOrNil: classIndexOrNil.
	classForeignObject nameForeignObject: (self findOrAddLiteral: aClassSpec name).
	classForeignObject instanceVariablesForeignObject: (self findOrAddLiteral: aClassSpec instVarNames).

	"This assumes quite a few things about Dictionary in the image we're about to create.
	Including that the #hash answered by Symbols should equal that of the mother image.
	We did better for IdentityDictionary, and in particular for the Smalltalk SystemDictionary.
	See #foreignIdentityHash.
	Consider doing something similar same for all Dictionaries...
		but how can we now what #hash will do in the new image?
	Other similar assumptions in #addSmalltalk #addMethodDictionaryFor: etc...
	For instance CompiledMethodForeignObject implements #identityHash,
	for the benefit of #addMethodDictionaryFor:"
	classPool := Dictionary new.
	aClassSpec classVarNames do: [ :cVarName |
		self findOrAddLiteral: cVarName.
		"Should we use ReadOnlyVariableBinding ? What's the point?"
		classPool at: cVarName put: nil ].
	"Create a ForeignObject for the class pool dictionary"
	classPoolForeignObject := self createForeignObjectFor: classPool.
	"Now replace the array inside with a ForeignObject for it"
	classPoolArray := classPool array.
	classPoolArrayForeignObject := self createForeignObjectFor: classPoolArray.
	classPool instVarNamed: 'array' put: classPoolArrayForeignObject.
	"Replace all associations with ForeignObjects too"
	classPoolArray withIndexDo: [ :association :index |
		association notNil ifTrue: [
			classPoolArray at: index put: (self createForeignObjectFor: association) ]].
	"Now give it to the class foreign object"
	classForeignObject classPoolForeignObject: classPoolForeignObject.

	"ClassSpec holds 'poolDictionaryNames', but it is currently unused. implement!!"
	classForeignObject sharedPoolsForeignObject: (self findForeignObjectFor: nil).

	^classForeignObject! !

!SpurImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 8/14/2023 16:37:10'!
addClassSpec: aClassSpec class: aForeignObjectClass classIndexOrNil: classIndexOrNil
	"Both for class and metaclass:
		aForeignObjectClass may be ClassForeignObject or MetaclassForeignObject"

	| hashAndClassIndex behaviorForeignObject rootsIdx pageIdx pageForeignObject page |
	hashAndClassIndex := classIndexOrNil ifNil: [self newClassIndex].
	rootsIdx := (hashAndClassIndex >> 10) + 1.
	pageIdx := (hashAndClassIndex bitAnd: (1 << 10 - 1)) + 1.
	pageForeignObject := classTableRoots at: rootsIdx.
	pageForeignObject isNil ifTrue: [
		pageForeignObject := self createForeignObjectFor: (Array new: 1024) hash: 0 class: SpurPseudoArrayForeignObject.
		classTableRoots at: rootsIdx put: pageForeignObject ].
	page := pageForeignObject representedObject.
	self assert: (page at: pageIdx) == nil.
	behaviorForeignObject := self createForeignObjectFor: aClassSpec hash: hashAndClassIndex class: aForeignObjectClass.
	page at: pageIdx put: behaviorForeignObject.
	^behaviorForeignObject! !

!SpurImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 6/23/2023 15:17:29'!
addClassTable
	"Add the ForeignObjects for:
		ClassTable+HiddenRoots
		FirstPateOfClassTable.
	See class comment at VMMaker SpurMemoryManager wrt 'hiddenRootsObj'. Also see #classTableRootSlots + #hiddenRootSlots."
	
	"Spur uses a maximum of 2ˆ12=4096 pages of 2ˆ10=1024 classes each.
	The classTablePages array and the first page array are always present.
	The rest of the pages are created on demand.
	Class index (that equals class identityHash) are usually small numbers, and few pages are actually needed."
	
	"These don't really need to be added to the foreignObjects dictionary,
	but doing it doesn't hurt, and lets us use #addForeignObjectFor: instead of adding extra code."

	| firstPageOfClassTable |
	classTableRoots := Array new: (2 raisedTo: 12) + 8 "#classTableRootSlots + #hiddenRootSlots".
	self createForeignObjectFor: classTableRoots hash: 0 class: SpurPseudoArrayForeignObject.
	"Spur reserves the first class index page immediately after the hiddenRoots, for #addForeignObjectsForVmKnownClasses"
	firstPageOfClassTable := Array new: 1024.
	classTableRoots at: 1 put: (	self createForeignObjectFor: firstPageOfClassTable hash: 0 class: SpurPseudoArrayForeignObject).
	"Add the second page too. Any other classes other than #addForeignObjectsForVmKnownClasses start here."
	firstPageOfClassTable := Array new: 1024.
	classTableRoots at: 2 put: (	self createForeignObjectFor: firstPageOfClassTable hash: 0 class: SpurPseudoArrayForeignObject).! !

!SpurImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 5/20/2023 10:23:43'!
addFreeLists
	self subclassResponsibility! !

!SpurImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 8/14/2023 16:45:49'!
addMetaclassFor: aClassSpec

	| metaclassForeignObject |
	metaclassForeignObject := self addClassSpec: aClassSpec class: MetaclassForeignObject classIndexOrNil: nil.
	metaclassForeignObject instanceVariablesForeignObject: (self findOrAddLiteral: aClassSpec classInstVarNames).
	^metaclassForeignObject! !

!SpurImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 8/17/2023 17:25:16'!
addMethodDictionaryFor: aClassOrMetaclassForeignObject
	"Create a ForeignObject for a new MethodDictionary to be created.
	Also create required ForeignObjects for its parts, i.e. the 'array' instance variable.
	Don't add the objects created to the foreignObjects dictionary, as nobody else should reference them!!"

	| methodForeignObjects methodDictionaryForeignObject methodDictionaryToAdd methodArrayForeignObject |
	methodForeignObjects := aClassOrMetaclassForeignObject methodForeignObjects.

	"This assumes that the representation of MethodDictionaries will be the same as in the image we are running in.
	Also, ForeignObjects for selectors must answer the same #identityHash as the to-be-created Symbol.
	So we can simply use a MethodDictionary.
	These assumptions may change in the future, and a more sophisticated strategy to build MethodDictionaries may be needed.
	This should actually take into account the source code from which the MethodDictionary class will be created..."
	methodDictionaryToAdd := MethodDictionary new: methodForeignObjects size.
	methodForeignObjects do: [ :each |
		methodDictionaryToAdd
			at: (self findForeignObjectFor: each representedObject selector)
			put: each ].

	"Give it to the argument Class or Metaclass."
	methodDictionaryForeignObject := self createForeignObjectFor: methodDictionaryToAdd.
	aClassOrMetaclassForeignObject methodDictForeignObject: methodDictionaryForeignObject.

	"The only additional ForeignObject is for the 'array' instance variable."	
	methodArrayForeignObject := self createForeignObjectFor: methodDictionaryToAdd methodArray.
	methodDictionaryToAdd instVarNamed: 'array' put: methodArrayForeignObject.

	^methodDictionaryForeignObject.! !

!SpurImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 8/17/2023 16:56:47'!
createForeignObjectFor: anObject hash: identityHash class: aForeignObjectClass
	"anObject will be included in the new image
	Create and answer an instance of aForeignObjectClass for it.
	For objects that are allocated in memory:
		- Allocate space in the new image for it
		- Add it to the list of foreign objects we'll use to serialize.
	For objects that won't be allocated in memory (they are immediate objects,
	encoded in pointing OOPs, like SmallInteger, etc):
		- Just create and answer a ForeignObject that can encode object in an OOP when needed."

	| fo byteCount |
	aForeignObjectClass instancesAreForImmediateObjects ifTrue: [
		self halt.
		].

	fo := aForeignObjectClass
		newFor: anObject
		oop: nextFreeAddressOfImage
		identityHash: identityHash.
	"If size > 254, header needs an extra word, and oop is pushed to point past it, to the basic header word"
	(self sizeFieldFor: fo) > 254 ifTrue: [
		fo foreignOop: nextFreeAddressOfImage + 8 ].

	"Size in memory. Some objects have specific slot width (like ByteArray).
	Others use the word size, like objects with instance variables and Arrays."
	byteCount := (self sizeInBytesOfHeader: fo) + (self sizeInBytesOfBody: fo).

"{'ForeignObject list node'. fo class. byteCount. anObject basicSize. nextFreeAddressOfImage. fo foreignOopFor: self. anObject} print."

	nextFreeAddressOfImage := nextFreeAddressOfImage + byteCount.

	firstForeignObject isNil
		ifTrue: [ firstForeignObject := fo ]
		ifFalse: [ lastForeignObject nextInList: fo ].
	lastForeignObject := fo.
	^fo! !

!SpurImageBuilder methodsFor: 'services' stamp: 'jmv 5/15/2023 16:26:26'!
newClassIndex
	"Answer the next class index to be used."

	lastClassIndex := lastClassIndex + 1.
	^lastClassIndex! !

!SpurImageBuilder methodsFor: 'services' stamp: 'jmv 5/22/2023 17:33:16'!
newObjectHash
	"Use a slight variation on D.H. Lehmer's linear congruential generator from 1951.
	 See e.g. http://en.wikipedia.org/wiki/Linear_congruential_generator.
	Copied from VMMaker SpurMemoryManager>>#newObjectHash"

	| hash |
	[
		"7 raisedTo: 5"
		lastHash := lastHash * 16807 bitAnd: 16rFFFFFFFF.
		 hash := lastHash + (lastHash >> 4). "adding the top bits gives much better spread."
		 (hash bitAnd: 16r3FFFFF) = 0 ] whileTrue.
	^hash bitAnd: 16r3FFFFF! !

!SpurImageBuilder methodsFor: 'serialization' stamp: 'jmv 8/17/2023 16:30:54'!
headerBytesFor: aForeignObject
	"Answer the total size in memory and in the generated image.
	Include header bytes, any padding, etc."

	| wordCount sizeHeaderByte answer extraHeader header classIndex |

	classIndex := aForeignObject spurClassIndexFor: self.

	"If size > 254, header needs an extra word, and oop is pushed to point past it, to the basic header word"
	wordCount := self sizeFieldFor: aForeignObject.
	sizeHeaderByte := wordCount <= 254
		ifTrue: [wordCount << 56]
		ifFalse: [16rFF00000000000000].
	header := sizeHeaderByte bitOr:
		((aForeignObject foreignIdentityHashFor: self) << 32)
		+ ((aForeignObject spurObjectFormatFor: self) << 24)
		+ classIndex.
	wordCount <= 254
		ifTrue: [
			answer := ByteArray new: 8.
			answer uint64At: 1 put: header bigEndian: isBigEndian.
			]
		ifFalse: [
			answer := ByteArray new: 16.
			extraHeader := sizeHeaderByte bitOr: wordCount.
			answer uint64At: 1 put: extraHeader bigEndian: isBigEndian.
			answer uint64At: 9 put: header bigEndian: isBigEndian.
			].
	^answer.! !

!SpurImageBuilder methodsFor: 'serialization' stamp: 'jmv 8/17/2023 16:49:47'!
sizeFieldFor: aForeignObject
	"Answer the size field for the object header.
	This is, the number of oop sized slots."

	| slotsPerWord |
	"Objects with 64 bit slots on Spur64, or 32 bit slots on Spur32"
	(aForeignObject slotWidth isNil or: [aForeignObject slotWidth = self wordSize]) ifTrue: [ ^ aForeignObject slotCountFor: self ].

	"For instance, objects with numeric 64 bit slots on Spur32"
	aForeignObject slotWidth > self wordSize ifTrue: [ ^ (aForeignObject slotCountFor: self) * (aForeignObject slotWidth // self wordSize) ].

	"Objects with smaller slots than oop size"
	slotsPerWord := self wordSize / aForeignObject slotWidth.
	^ (aForeignObject slotCountFor: self) + slotsPerWord-1 // slotsPerWord! !

!SpurImageBuilder methodsFor: 'serialization' stamp: 'jmv 8/17/2023 17:13:33'!
sizeInBytesOfBody: aForeignObject
	"Answer the byte size of argument, in memory and in the generated image.
	Include any padding."

	| byteCount |

	"No object is smaller than 8 bytes.
	This is to have enough room the VM to turn it into a #become forwarder if needed.
	Include the filler (see below) for Spur32. Both in Spur32 and Spur64 write 8 bytes."
	(aForeignObject slotCountFor: self) = 0 ifTrue: [
		^8 ].

	"Size in memory. Some objects have specific slot width (like ByteArray). Others use the word size, like objects with instance variables and Arrays."
	byteCount := (aForeignObject slotCountFor: self) * (aForeignObject slotWidth ifNil: [ self wordSize ]).

	"Filler. Round to next multiple of 8, both Spur32 and Spur64 allocate multiples of 64 bits"
	byteCount := byteCount + 7 // 8 * 8.

	^byteCount.! !

!SpurImageBuilder methodsFor: 'serialization' stamp: 'jmv 8/17/2023 16:55:15'!
sizeInBytesOfHeader: aForeignObject
	"Answer the size of the header for argument, in memory and in the generated image."

	"If size > 254, header needs an extra 64 bits word, and oop is pushed to point past it, to the basic header word"
	(self sizeFieldFor: aForeignObject) > 254 ifTrue: [^16 ].

	"Regular header is 8 bytes."
	^8! !

!SpurImageBuilder methodsFor: 'format constants' stamp: 'jmv 8/22/2023 13:34:39'!
lastBridge
	self subclassResponsibility! !

!SpurImageBuilder methodsFor: 'format constants' stamp: 'jmv 5/23/2023 16:30:25'!
maxIdentityHash
	"Answer the maximum identityHash value supported by the VM.
	22 bits in Spur"

	^16r3FFFFF! !

!SpurImageBuilder methodsFor: 'image file saving' stamp: 'jmv 8/22/2023 15:43:51'!
saveImageTo: aFileEntry

	| ba nextForeignObject p |
	ba := ByteArray
		streamContents: [ :byteStream |
			byteStream nextPutAll: (ByteArray new: baseAddressOfImage).

			"Iterate the linked list of foreign objects. Write each."
			nextForeignObject := firstForeignObject.
			[ nextForeignObject notNil ] whileTrue: [
				"asegurarse que la clase esté o dar error."
				(self sizeFieldFor: nextForeignObject) <= 254
					ifTrue: [
						"For objects with only basic header word, oop points to it."
						self assert: byteStream position = (nextForeignObject foreignOopFor: self) ]
					ifFalse: [
						"For objects with an extra header before the basic header word, oop points to basic header word."
						self assert: byteStream position = ((nextForeignObject foreignOopFor: self)-8) ].
				self write: nextForeignObject on: byteStream.
				nextForeignObject := nextForeignObject nextInList ].	

			"Write last segment bridge"
			byteStream nextUint64Put: self lastBridge bigEndian: isBigEndian.
			byteStream nextUint64Put: 0 bigEndian: isBigEndian.

			"Write image header"
			p := byteStream position.
			byteStream position: 0.
			self writeImageFileHeaderOn: byteStream byteSize: p.
			byteStream position: p.
		]
		estimatedSize: nextFreeAddressOfImage.
	aFileEntry forceWriteStreamDo: [ :strm | strm binary; nextPutAll: ba ]! !

!SpurImageBuilder methodsFor: 'image file saving' stamp: 'jmv 8/22/2023 12:19:28'!
write: aForeignObject on: byteStream

	| headerBytes |

	"Object header"
	headerBytes := self headerBytesFor: aForeignObject.
	byteStream nextPutAll: headerBytes.

	"No object is smaller than 8 bytes.
	This is to have enough room the VM to turn it into a #become forwarder if needed.
	Include the filler (see below) for Spur32. Both in Spur32 and Spur64 write 8 bytes."
	(aForeignObject slotCountFor: self) = 0 ifTrue: [
		byteStream nextPutAll: `ByteArray new: 8`.
		^self ].

	"Object body."
	aForeignObject writeSlotsOn: byteStream forSpur: self.

	"Appropriate filler. All objects use a multiple of 8 bytes.
	Therefore, stream position must also be a multiple of 8.
	Note: This matches #sizeInBytesOfBody:"
	[(byteStream position \\ 8) = 0] whileFalse: [byteStream nextPut: 0].! !

!Spur32ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 6/23/2023 15:17:33'!
addFreeLists
	"
	x-x-x-x-x-x-
	
	Create a ForeignObject for it.
	Add it to the list of foreign objects we'll use to serialize.
	Don't add it to the foreignObjects dictionary!!"

	self createForeignObjectFor: nil hash: 0 class: Spur32FreeListsForeignObject.! !

!Spur32ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 8/15/2023 18:29:55'!
foreignObjectClassFor: anObject
	"Answer the correct ForeignObject subclass to represent anObject.
	"
	anObject isInteger ifTrue: [
		^(anObject between: `(2 raisedTo: 30) negated` and: `(2 raisedTo: 30)-1`)
			ifTrue: [ SmallIntegerForeignObject ]
			ifFalse: [ LargeIntegerForeignObject ]].

	anObject isFloat ifTrue: [
		^BoxedFloat64ForeignObject ].

	anObject class == Character ifTrue: [
		^ImmediateCharacterForeignObject ].
		
	^super foreignObjectClassFor: anObject! !

!Spur32ImageBuilder methodsFor: 'services' stamp: 'jmv 8/17/2023 14:37:38'!
foreignOopOfImmediateCharacter: aCharacter

	| numericValue |
	self assert: aCharacter class == Character. 
	numericValue := aCharacter numericValue.
	self assert: numericValue <= 255.
	^(numericValue << 2) + 2.! !

!Spur32ImageBuilder methodsFor: 'services' stamp: 'jmv 6/29/2023 16:27:03'!
foreignOopOfSmallInteger: aNumber

	"Check that aNumber can actually be encoded as a SmallInteger for us."
	self assert: aNumber <= `(2 raisedTo: 30)-1`.
	self assert: aNumber >= `(2 raisedTo: 30) negated`.
	^aNumber < 0
		ifTrue: [((16r80000000 + aNumber) << 1) + 1]
		ifFalse: [(aNumber << 1) + 1]! !

!Spur32ImageBuilder methodsFor: 'services' stamp: 'jmv 8/17/2023 15:46:24'!
on: aStream nexWordPut: aNumber
	"Write #wordSize bytes. Use 'isBigEndian' endianess."

	self on: aStream nextUint32Put: aNumber.! !

!Spur32ImageBuilder methodsFor: 'format constants' stamp: 'jmv 5/10/2023 12:06:56'!
imageFormat
	"Answer the number that identifies this image format"
	^6521! !

!Spur32ImageBuilder methodsFor: 'format constants' stamp: 'jmv 5/10/2023 12:23:08'!
imageHeaderSize
	"In Bytes"
	^64! !

!Spur32ImageBuilder methodsFor: 'format constants' stamp: 'jmv 8/22/2023 13:36:20'!
lastBridge	
	"Memory segment last bridge (See VMMaker - SpurMemoryManager class comment)
	See VMMaker - #initSegmentBridgeWithBytes:at:
	See VMMaker - #initializeObjectMemory:
	For 32 bit Spur:
		MarkedBit: 1<<55
		PinedBit: 1<< 30
		Format: A (32 bit indexable)
		ClassIndex: 3 (#segmentBridgePun)"

	^16r8000004A000003! !

!Spur32ImageBuilder methodsFor: 'format constants' stamp: 'jmv 5/10/2023 12:26:31'!
wordSize
	"Also size of an oop.
	In Bytes"
	^4! !

!Spur64ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 6/23/2023 15:17:41'!
addFreeLists
	"
	x-x-x-x-x-x-
	
	Create a ForeignObject for it.
	Add it to the list of foreign objects we'll use to serialize.
	Don't add it to the foreignObjects dictionary!!"

	self createForeignObjectFor: nil hash: 0 class: Spur64FreeListsForeignObject.! !

!Spur64ImageBuilder methodsFor: 'foreign objects' stamp: 'jmv 8/15/2023 18:29:59'!
foreignObjectClassFor: anObject
	"Answer the correct ForeignObject subclass to represent anObject.
	"
	anObject isInteger ifTrue: [
		^(anObject between: `(2 raisedTo: 60) negated` and: `(2 raisedTo: 60)-1`)
			ifTrue: [ SmallIntegerForeignObject ]
			ifFalse: [ LargeIntegerForeignObject ]].

	anObject isFloat ifTrue: [
		anObject exponentBits = 0 ifTrue: [
			^anObject isDenormalized
				ifFalse: [ SmallFloat64ForeignObject ] "0.0"
				ifTrue: [ BoxedFloat64ForeignObject ] "Denormals" ].
		^(anObject exponentBits - 1023 between: -127 and: 128)
			ifTrue: [ SmallFloat64ForeignObject ]
			ifFalse: [ BoxedFloat64ForeignObject ]].

	anObject class == Character ifTrue: [
		^ImmediateCharacterForeignObject ].
		
	^super foreignObjectClassFor: anObject! !

!Spur64ImageBuilder methodsFor: 'services' stamp: 'jmv 8/17/2023 14:37:40'!
foreignOopOfImmediateCharacter: aCharacter

	| numericValue |
	self assert: aCharacter class == Character. 
	numericValue := aCharacter numericValue.
	self assert: numericValue <= 255.
	^(numericValue << 3) + 2.! !

!Spur64ImageBuilder methodsFor: 'services' stamp: 'jmv 7/19/2023 15:57:10'!
foreignOopOfSmallFloat64: aFloat

	"Check that aNumber can actually be encoded as a SmalFloat64 for us."
	| answer |
	self deny: aFloat isDenormalized.
	self assert: (aFloat isZero or: [ aFloat exponentBits - 1023 between: -127 and: 128 ]).
	aFloat partBits: [ :signBit :exponentBits :mantissaBits | | exponentBits8 |
		exponentBits8 := exponentBits ~= 0
			ifTrue: [exponentBits - 1023 + 127]
			ifFalse: [0].
		answer := (mantissaBits << 4)
			+ (signBit << 3)
			+ 4
			+ ((exponentBits8 bitAnd: 16rFF) << 56) ].
	^answer! !

!Spur64ImageBuilder methodsFor: 'services' stamp: 'jmv 7/4/2023 20:01:31'!
foreignOopOfSmallInteger: aNumber

	"Check that aNumber can actually be encoded as a SmallInteger for us."
	self assert: aNumber <= `(2 raisedTo: 60)-1`.
	self assert: aNumber >= `(2 raisedTo: 60) negated`.
	^aNumber < 0
		ifTrue: [((16r2000000000000000 + aNumber) << 3) + 1]
		ifFalse: [(aNumber << 3) + 1]! !

!Spur64ImageBuilder methodsFor: 'services' stamp: 'jmv 8/17/2023 15:46:36'!
on: aStream nexWordPut: aNumber
	"Write #wordSize bytes. Use 'isBigEndian' endianess."

	self on: aStream nextUint64Put: aNumber.! !

!Spur64ImageBuilder methodsFor: 'format constants' stamp: 'jmv 5/10/2023 12:07:13'!
imageFormat
	"Answer the number that identifies this image format"
	^68021! !

!Spur64ImageBuilder methodsFor: 'format constants' stamp: 'jmv 5/10/2023 12:23:13'!
imageHeaderSize
	"In Bytes"
	^128! !

!Spur64ImageBuilder methodsFor: 'format constants' stamp: 'jmv 8/22/2023 13:34:28'!
lastBridge	
	"Memory segment last bridge (See VMMaker - SpurMemoryManager class comment)
	See VMMaker - #initSegmentBridgeWithBytes:at:
	See VMMaker - #initializeObjectMemory:
	For 64 bit Spur:
		MarkedBit: 1<<55
		PinedBit: 1<< 30
		Format: 9 (64 bit indexable)
		ClassIndex: 3 (#segmentBridgePun)"

	^16r80000049000003! !

!Spur64ImageBuilder methodsFor: 'format constants' stamp: 'jmv 5/10/2023 12:26:34'!
wordSize
	"Also size of an oop.
	In Bytes"
	^8! !

!V3ImageBuilder methodsFor: 'initialization' stamp: 'jmv 5/15/2023 11:53:32'!
initialize
	super initialize.
	lastHash := 999.! !

!V3ImageBuilder methodsFor: 'image building' stamp: 'jmv 7/17/2023 12:48:14'!
fillSpecialObjectsArray
	"Please see implementation for Spur.
	Also see #recreateSpecialObjectsArray.
	See reference implementations (MicroSqueak.pck.st and SystemTracing.pck.st)"

	self error: 'Must implement'! !

!V3ImageBuilder methodsFor: 'services' stamp: 'jmv 5/15/2023 12:03:55'!
newObjectHash
	"Answer a new 16-bit pseudo-random number for use as an identity hash.
	Essentially copied from VMMaker ObjectMemory>>#newObjectHash, answer lower 12 bits"

	lastHash := 13849 + (27181 * lastHash) bitAnd: 65535.
	^ lastHash bitAnd: 16rFFF! !

!V3ImageBuilder methodsFor: 'services' stamp: 'jmv 8/17/2023 15:46:44'!
on: aStream nexWordPut: aNumber
	"Write #wordSize bytes. Use 'isBigEndian' endianess."

	self on: aStream nextUint32Put: aNumber.! !

!V3ImageBuilder methodsFor: 'format constants' stamp: 'jmv 5/10/2023 14:15:21'!
imageFormat
	"Answer the number that identifies this image format"
	^6505! !

!V3ImageBuilder methodsFor: 'format constants' stamp: 'jmv 5/10/2023 14:15:25'!
imageHeaderSize
	"In Bytes"
	^64! !

!V3ImageBuilder methodsFor: 'format constants' stamp: 'jmv 5/23/2023 16:30:45'!
maxIdentityHash
	"Answer the maximum identityHash value supported by the VM.
	12 bits in V3 images"

	^16rFFF! !

!V3ImageBuilder methodsFor: 'format constants' stamp: 'jmv 5/10/2023 14:15:29'!
wordSize
	"Also size of an oop.
	In Bytes"
	^4! !

!ForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 10:53:29'!
foreignIdentityHashFor: anImageBuilder
	"IdentityHash to be used in the newly created image."

	^self subclassResponsibility! !

!ForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 10:53:58'!
foreignOopFor: anImageBuilder
	"Oop (Object Pointer) to be used in the newly created image."

	^self subclassResponsibility! !

!ForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 15:27:05'!
nameOfClassToInstantiate: aSymbol
	nameOfClassToInstantiate := aSymbol! !

!ForeignObject methodsFor: 'accessing' stamp: 'jmv 5/16/2023 11:00:05'!
representedObject

	^representedObject! !

!ForeignObject methodsFor: 'accessing' stamp: 'jmv 5/19/2023 17:37:58'!
slotCount
	"Answer the number of slots used by our representedObject"

	^self subclassResponsibility! !

!ForeignObject methodsFor: 'accessing' stamp: 'jmv 8/1/2023 15:09:26'!
slotCountFor: anImageBuilder
	"Answer the number of slots used by our representedObject.
	Redefined where #slotCount is meaningless without anImageBuilder."

	^self slotCount! !

!ForeignObject methodsFor: 'accessing' stamp: 'jmv 5/19/2023 18:26:21'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	If our slots are oops, and therefore word size, answer nil."

	^self subclassResponsibility! !

!ForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 10:55:24'!
spurClassIndexFor: aSpurImageBuilder
	"Use argument if appropriate to fetch a ClassForeignObject for us.
	Answer its #foreignIdentityHash.
	May be redefined by subclasses."

	^(aSpurImageBuilder findClassNamed: nameOfClassToInstantiate) foreignIdentityHashFor: aSpurImageBuilder! !

!ForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:08:49'!
spurObjectFormat
	"Answer the format 5 bit field for Object Header in Spur."

	^self subclassResponsibility! !

!ForeignObject methodsFor: 'accessing' stamp: 'jmv 8/1/2023 15:48:31'!
spurObjectFormatFor: aSpurImageBuilder
	"Answer the format 5 bit field for Object Header in Spur."

	^self spurObjectFormat! !

!ForeignObject methodsFor: 'serialization' stamp: 'jmv 8/17/2023 15:34:10'!
writeSlotsOn: byteStream forSpur: anImageBuilder

	self subclassResponsibility.! !

!ForeignObject methodsFor: 'testing' stamp: 'jmv 6/22/2023 15:54:13'!
is: aSymbol

	^ aSymbol == #ForeignObject or: [ super is: aSymbol ]! !

!ForeignObject methodsFor: 'services' stamp: 'jmv 6/29/2023 16:47:12'!
referencedObjectsDo: aBlock
	"Evaluate aBlock for each object our representedObject points to.
	To be redefined in classes whose representedObject actually holds OOP slots, like Arrays or object with instance variables.
	Also to be redefined as NOP on classes whose representedObjects don't hold pointers at all."

	self halt: 'think what to do!!'! !

!ForeignObject methodsFor: 'printing' stamp: 'jmv 7/17/2023 11:58:18'!
printOn: aStream
	aStream
		nextPutAll: self class name withArticle;
		nextPutAll: '. Foreign class named: '; nextPutAll: nameOfClassToInstantiate;
		nextPutAll: '. Object: '.
	self printRepresentedObjectOn: aStream.! !

!ForeignObject methodsFor: 'printing' stamp: 'jmv 7/14/2023 09:16:13'!
printRepresentedObjectOn: aStream
	aStream nextPutAll: representedObject shortPrintString! !

!ForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 5/15/2023 16:40:31'!
canRepresent: anObject
	"Subclasses should answer true for objects they can represent."

	^false! !

!ForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 6/23/2023 17:19:33'!
subclassFor: anObject
	"Find a subclass that can handle it.
	Note: Errors for Characters, Integers and Floats. Senders beware."

	self allSubclassesDo: [ :subclass |
		(subclass canRepresent: anObject) ifTrue: [ ^subclass ]].
	self error: 'None found'! !

!ForeignObject class methodsFor: 'testing' stamp: 'jmv 6/23/2023 17:33:57'!
instancesAreForImmediateObjects
	"Answer true if our instances represent objects that will not be allocated in a new image,
	because they will be immediate, and fully encoded in their OOP."

	self subclassResponsibility! !

!AllocatedForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 10:53:33'!
foreignIdentityHashFor: anImageBuilder
	"IdentityHash to be used in the newly created image."

	^identityHash! !

!AllocatedForeignObject methodsFor: 'accessing' stamp: 'jmv 5/19/2023 17:52:38'!
foreignOop: aNumber
	oop := aNumber! !

!AllocatedForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 10:54:01'!
foreignOopFor: anImageBuilder

	^oop! !

!AllocatedForeignObject methodsFor: 'accessing' stamp: 'jmv 5/16/2023 09:24:25'!
nextInList
	^nextInList! !

!AllocatedForeignObject methodsFor: 'accessing' stamp: 'jmv 5/16/2023 09:24:30'!
nextInList: aForeignObject
	nextInList := aForeignObject! !

!AllocatedForeignObject methodsFor: 'accessing' stamp: 'jmv 6/23/2023 17:58:20'!
representedObject: aLocalObject oop: newOop identityHash: newIdentityHash

	representedObject := aLocalObject.
	nameOfClassToInstantiate := aLocalObject class name.
	oop := newOop.
	identityHash := newIdentityHash.
	nextInList := nil.! !

!AllocatedForeignObject class methodsFor: 'instance creation' stamp: 'jmv 6/23/2023 15:11:55'!
newFor: aLocalObject oop: newOop identityHash: newIdentityHash
	"Answer an instance of the correct subclass."

	^self new representedObject: aLocalObject oop: newOop identityHash: newIdentityHash! !

!AllocatedForeignObject class methodsFor: 'testing' stamp: 'jmv 6/23/2023 17:34:09'!
instancesAreForImmediateObjects
	"Answer true if our instances represent objects that will not be allocated in a new image,
	because they will be immediate, and fully encoded in their OOP."

	^false! !

!BoxedFloat64ForeignObject methodsFor: 'accessing' stamp: 'jmv 7/18/2023 12:48:50'!
slotCount
	"Answer the number of slots used by our representedObject"

	^2! !

!BoxedFloat64ForeignObject methodsFor: 'accessing' stamp: 'jmv 7/18/2023 12:48:02'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Our slots are int32 or uint32."

	^4! !

!BoxedFloat64ForeignObject methodsFor: 'accessing' stamp: 'jmv 8/1/2023 15:49:07'!
spurObjectFormat
	"Answer the format 5 bit field for Object Header in Spur."

	^10! !

!BoxedFloat64ForeignObject methodsFor: 'serialization' stamp: 'jmv 8/17/2023 15:47:16'!
writeSlotsOn: byteStream forSpur: anImageBuilder

	"Actual data
	Note: #basicAt: answers as if the Float was in BigEndian, for historical reasons."
	anImageBuilder on: byteStream nextUint32Put: (representedObject basicAt: 2).
	anImageBuilder on: byteStream nextUint32Put: (representedObject basicAt: 1).! !

!BoxedFloat64ForeignObject methodsFor: 'services' stamp: 'jmv 7/18/2023 12:50:40'!
referencedObjectsDo: aBlock
	"Evaluate aBlock for each object our representedObject points to.
	We don't reference any other objects."! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 6/2/2023 14:56:13'!
addMethod: aMethodForeignObject
	methodForeignObjects add: aMethodForeignObject ! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 15:25:57'!
addSubclass: aClassForeignObject
	subclassForeignObjects add: aClassForeignObject ! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 14:52:10'!
classPoolForeignObject: aForeignObject
	classPoolForeignObject := aForeignObject! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 14:50:21'!
instanceVariablesForeignObject: aForeignObject
	instanceVariablesForeignObject := aForeignObject! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 6/2/2023 15:33:51'!
metaclassForeignObject
	^metaclassForeignObject! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 5/16/2023 15:11:46'!
metaclassForeignObject: aMetaclassForeignObject
	metaclassForeignObject := aMetaclassForeignObject! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 5/16/2023 15:33:52'!
methodDictForeignObject: aMethodDictionaryForeignObject
	methodDictForeignObject := aMethodDictionaryForeignObject! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 6/5/2023 15:39:49'!
methodForeignObjects
	^methodForeignObjects! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 14:51:51'!
nameForeignObject: aForeignObject
	nameForeignObject := aForeignObject! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 6/23/2023 18:15:31'!
representedObject: aLocalObject oop: newOop identityHash: newIdentityHash
	super representedObject: aLocalObject oop: newOop identityHash: newIdentityHash.
	nameOfClassToInstantiate := aLocalObject name, ' class'.! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 14:52:51'!
sharedPoolsForeignObject: aForeignObject
	sharedPoolsForeignObject := aForeignObject! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 8/25/2023 14:28:44'!
slotCount
	"Answer the number of slots used by our representedObject"

	"Assuming standard representation in Cuis / Squeak.
	Refector as needed when this no longer true, to reflect the source code for the image being built!!
	Class allInstVarNames size = 9"
	^ 9 + representedObject totalClassInstVarCountInSpec! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 5/20/2023 13:55:03'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Answer nil. Our slots are oops."

	^nil! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 10:55:34'!
spurClassIndexFor: aSpurImageBuilder
	"We already know the foreign object for our metaclass"

	^metaclassForeignObject foreignIdentityHashFor: aSpurImageBuilder! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:11:44'!
spurObjectFormat
	"Answer the format 5 bit field for Object Header in Spur."

	^1! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 15:48:37'!
subclassForeignObjects
	^subclassForeignObjects! !

!ClassForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 14:51:32'!
subclassesForeignObject: aForeignObject
	subclassesForeignObject := aForeignObject! !

!ClassForeignObject methodsFor: 'initialization' stamp: 'jmv 6/26/2023 15:25:03'!
initialize

	super initialize.
	methodForeignObjects := OrderedCollection new.
	subclassForeignObjects := OrderedCollection new.! !

!ClassForeignObject methodsFor: 'serialization' stamp: 'jmv 8/25/2023 14:55:35'!
writeSlotsOn: byteStream forSpur: anImageBuilder

	| superclassForeignObject format |

	"Superclass oop"
	superclassForeignObject := anImageBuilder findForeignObjectFor: representedObject superclass.
	anImageBuilder on: byteStream nexWordPut: (superclassForeignObject foreignOopFor: anImageBuilder).

	"methodDict oop"
	anImageBuilder on: byteStream nexWordPut: (methodDictForeignObject foreignOopFor: anImageBuilder).

	"format oop"
	 format := ClassBuilder new computeFormat: representedObject typeOfClass fullInstancesSize: representedObject allInstVarCountInSpec.
"Only valid when exactly cloning classes, and running on Spur too"
self assert: (Smalltalk at: representedObject name) format = format.
	anImageBuilder on: byteStream nexWordPut: (anImageBuilder foreignOopOfSmallInteger: format).

	"instanceVariables oop"
	anImageBuilder on: byteStream nexWordPut: (instanceVariablesForeignObject foreignOopFor: anImageBuilder).

	"organization oop"
	anImageBuilder on: byteStream nexWordPut: ((anImageBuilder findForeignObjectFor: nil) foreignOopFor: anImageBuilder).

	"subclasses oop"
	anImageBuilder on: byteStream nexWordPut: (subclassesForeignObject foreignOopFor: anImageBuilder).

	"name oop"
	anImageBuilder on: byteStream nexWordPut: (nameForeignObject foreignOopFor: anImageBuilder).

	"classPool oop"
	anImageBuilder on: byteStream nexWordPut: (classPoolForeignObject foreignOopFor: anImageBuilder).

	"sharedPools oop"
	anImageBuilder on: byteStream nexWordPut: (sharedPoolsForeignObject foreignOopFor: anImageBuilder).

	"class instance variables oops
	Initialize them to nil"
	representedObject classInstVarNames size timesRepeat: [
		anImageBuilder on: byteStream nexWordPut: ((anImageBuilder findForeignObjectFor: nil) foreignOopFor: anImageBuilder) ].! !

!ClassForeignObject methodsFor: 'services' stamp: 'jmv 7/6/2023 09:58:57'!
localBindingOf: varNameSymbol for: anImageBuilder
	"Answer the foreign object for the Association that holds class var named as the argument.
	Answer nil if no such class variable.
	We assume that the class pool object is both valid here and in the new image. That means that
	Symbol >> #hash will answer the same in both. This is quite an assumption!!"

	(classPoolForeignObject representedObject array representedObject  								"its #array"
			detect: [ :associationForeignObject |
				associationForeignObject notNil and: [
					associationForeignObject representedObject key
						= varNameSymbol ] ]
			ifNone: nil) ifNotNil: [ :bindingForeignObject |
				^ bindingForeignObject ].

	"Finally look higher up the superclass chain and fail at the end."
	^representedObject superclass ifNotNil: [ :superclassSpec |
		(anImageBuilder findForeignObjectFor: superclassSpec)
			localBindingOf: varNameSymbol for: anImageBuilder ]! !

!ClassForeignObject methodsFor: 'printing' stamp: 'jmv 7/14/2023 09:20:06'!
printRepresentedObjectOn: aStream
	aStream nextPutAll: representedObject name.! !

!CompiledMethodForeignObject methodsFor: 'accessing' stamp: 'jmv 6/2/2023 15:27:28'!
methodClassForeignObject: aForeignObject
	"Argument is either a ClassForeignObject or a MetaclassForeignObject"
	methodClassForeignObject := aForeignObject.! !

!CompiledMethodForeignObject methodsFor: 'accessing' stamp: 'jmv 6/23/2023 18:18:07'!
representedObject: aLocalObject oop: newOop identityHash: newIdentityHash
	super representedObject: aLocalObject oop: newOop identityHash: newIdentityHash.
	nameOfClassToInstantiate := #CompiledMethod! !

!CompiledMethodForeignObject methodsFor: 'accessing' stamp: 'jmv 8/1/2023 15:06:33'!
slotCount
	"Our number of slots depends also on the image format and builder.
	see #slotCountFor:
	"
	self shouldNotImplement! !

!CompiledMethodForeignObject methodsFor: 'accessing' stamp: 'jmv 8/17/2023 16:02:26'!
slotCountFor: anImageBuilder
	"Answer the number of slots used by our representedObject.
	Total number of bytes required by this CompiledMethod.
	Ask the imageBuilder. It depends on the image format!!"

	^self usedBytesFor: anImageBuilder.! !

!CompiledMethodForeignObject methodsFor: 'accessing' stamp: 'jmv 6/2/2023 15:41:25'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Our slots are int8 or uint8."

	^1! !

!CompiledMethodForeignObject methodsFor: 'accessing' stamp: 'jmv 8/17/2023 16:02:46'!
spurObjectFormatFor: aSpurImageBuilder
	"Answer the format 5 bit field for Object Header in Spur.
	Use  or 3 2 low bits of size. Answer in #(24 25 26 27) for Spur32 and in #(24 25 26 27 28 29 30 31) for Spur64."

	| usedBytes oddBits bytesPerWord |
	usedBytes := self usedBytesFor: aSpurImageBuilder.
	bytesPerWord := aSpurImageBuilder wordSize.
	oddBits := ((usedBytes / bytesPerWord) ceiling * bytesPerWord) - usedBytes.
	^24 bitOr: oddBits! !

!CompiledMethodForeignObject methodsFor: 'accessing' stamp: 'jmv 8/17/2023 16:02:13'!
usedBytesFor: anImageBuilder
	"Answer the number of bytes used by our representedObject.
	Total number of bytes required by this CompiledMethod.
	Ask the imageBuilder. It depends on the image format!!"
	
	^(representedObject spurNumLiterals + 1) * anImageBuilder wordSize
		+ representedObject bytecodes size
		+ 4.! !

!CompiledMethodForeignObject methodsFor: 'serialization' stamp: 'jmv 9/20/2023 16:04:50'!
writeSlotsOn: byteStream forSpur: anImageBuilder

	| classBinding methodSelectorForeignObject |

	"No habria que calcular esto? Es como que la Spec está atada a Spur, o a la imagen que la generó. Habrá que limpiar."
	anImageBuilder on: byteStream nexWordPut: (anImageBuilder foreignOopOfSmallInteger: representedObject header).

	"Regular literals (CompiledMethods have two special last literals, see below)"
	representedObject literals withIndexDo: [ :lit :i |
		(lit is: #LiteralVariableSpec)
			ifTrue: [
					"Add reference to variable binding:
					- the Association in the classPool (for class variables)
					- the Association in the SystemDictionary (for global variables)"
					(methodClassForeignObject localBindingOf: lit variableName for: anImageBuilder)
						ifNotNil: [ :assocForeignObject |
							"{'local binding'. lit variableName. assocForeignObject representedObject. assocForeignObject representedObject identityHash} print."
							anImageBuilder on: byteStream nexWordPut: (assocForeignObject foreignOopFor: anImageBuilder) ]
						ifNil: [
							(anImageBuilder findGlobalBindingNamed: lit variableName)
								ifNotNil: [ :assocForeignObject2 |
									"{'global binding'. lit variableName. assocForeignObject2 representedObject identityHash} print."
									anImageBuilder on: byteStream nexWordPut: (assocForeignObject2 foreignOopFor: anImageBuilder) ]
								ifNil: [
									self error: 'Could not bind variable name: ',  lit variableName printString ]]]
			ifFalse: [ | literalForeignObject |
				literalForeignObject := (lit is: #ForeignObject)
					ifTrue: [ lit halt: 'any? (shouldn"t)' ]
					ifFalse: [ anImageBuilder findForeignObjectFor: lit ].
				anImageBuilder on: byteStream nexWordPut: (literalForeignObject foreignOopFor: anImageBuilder) ]].

	"#penultimateLiteral, either method selector or an AdditionalMethodState that also holds Pragmas and/or Properties(Associations)"
	methodSelectorForeignObject := anImageBuilder findForeignObjectFor: representedObject selector.
	representedObject pragmaSpecs isEmpty
		ifTrue: [
			anImageBuilder on: byteStream nexWordPut: (methodSelectorForeignObject foreignOopFor: anImageBuilder) ]
		ifFalse: [ | additionalState additionalMethodStateForeignObject |
			"This assumes that the representation of AdditionalMethodState is the same as in the image we're running in.
			We may need to improve this for proper compatibility between different kinds of mother / child image.
			Same for Pragma."
			additionalState := AdditionalMethodState basicNew: representedObject pragmaSpecs size.
			additionalState selector: methodSelectorForeignObject.
			additionalState method: self.
			representedObject pragmaSpecs withIndexDo: [ :pragmaData :i |
				| pragma pragmaKeyword pragmaArguments |
				pragmaKeyword := pragmaData first.
				pragmaArguments := pragmaData copyFrom: 2 to: pragmaData size.
				"Or understand and implement!!"
				self assert: pragmaArguments isLiteral.
				pragma := Pragma
					for: self
					selector: (anImageBuilder findOrAddLiteral: pragmaKeyword)
					arguments: (anImageBuilder findOrAddLiteral: pragmaArguments).
				additionalState basicAt: i put: (anImageBuilder createForeignObjectFor: pragma) ].
			additionalMethodStateForeignObject := anImageBuilder createForeignObjectFor: additionalState.
			anImageBuilder on: byteStream nexWordPut: (additionalMethodStateForeignObject foreignOopFor: anImageBuilder) ].

	"Last literal is #methodClass"
	self assert:({ClassForeignObject . MetaclassForeignObject} includes: 	methodClassForeignObject class).
	methodClassForeignObject class == ClassForeignObject ifTrue: [
		self assert: representedObject classSpec == methodClassForeignObject representedObject.
		self assert: (anImageBuilder findForeignObjectFor: representedObject classSpec) == methodClassForeignObject.
		"aClass binding"
		classBinding := anImageBuilder findGlobalBindingNamed: representedObject classSpec name.
		anImageBuilder on: byteStream nexWordPut: (classBinding foreignOopFor: anImageBuilder) ].
	methodClassForeignObject class == MetaclassForeignObject ifTrue: [
		self assert: representedObject classSpec == methodClassForeignObject thisClassForeignObject representedObject.
		self assert: (anImageBuilder findForeignObjectFor: representedObject classSpec) == methodClassForeignObject thisClassForeignObject.
		"aMetaclass binding"
		classBinding := anImageBuilder createForeignObjectFor: (Association key: nil value: methodClassForeignObject).
		anImageBuilder on: byteStream nexWordPut: (classBinding foreignOopFor: anImageBuilder) ].

	"Next add bytecodes"
	byteStream nextPutAll: representedObject bytecodes.

	"Method Trailer.
	Used for the source code pointer. See #setSourcePointer:
	Just zeros unless we add source code."
	byteStream nextPutAll: #(0 0 0 0).! !

!CompiledMethodForeignObject methodsFor: 'compatibility' stamp: 'jmv 7/13/2023 15:42:38'!
flushCache
	"Needed to enable adding us to MethodDictionaries"! !

!CompiledMethodForeignObject methodsFor: 'printing' stamp: 'jmv 7/14/2023 09:18:55'!
printRepresentedObjectOn: aStream
	aStream nextPutAll: representedObject classSpec name.
	representedObject isClassSide ifTrue: [
		aStream nextPutAll: ' class' ].
	aStream nextPutAll: '>>#'; nextPutAll: representedObject selector.! !

!CompiledMethodForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 6/7/2023 17:35:37'!
canRepresent: anObject
	"We can represent instances of 	MethodSpec."

	^anObject class == MethodSpec! !

!Indexed16BitForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:35:32'!
slotCount
	"Answer the number of slots used by our representedObject"

	^representedObject basicSize! !

!Indexed16BitForeignObject methodsFor: 'accessing' stamp: 'jmv 5/23/2023 09:49:48'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Our slots are int16 or uint16."

	^2! !

!Indexed16BitForeignObject methodsFor: 'accessing' stamp: 'jmv 8/1/2023 15:38:26'!
spurObjectFormatFor: aSpurImageBuilder
	"Answer the format 5 bit field for Object Header in Spur.
	Use 1 or 2 low bits of size. Answer in #(12 13) for Spur32 and in #(12 13 14 15) for Spur64."

	| slotCount oddBits slotsPerWord |
	slotCount := self slotCount.
	slotsPerWord := aSpurImageBuilder wordSize // 2.
	oddBits := ((slotCount / slotsPerWord) ceiling * slotsPerWord) - slotCount.
	^12 bitOr: oddBits! !

!Indexed16BitForeignObject methodsFor: 'serialization' stamp: 'jmv 8/17/2023 17:15:17'!
writeSlotsOn: byteStream forSpur: anImageBuilder

	"Actual data"
	1 to: self slotCount do: [ :i |
		anImageBuilder on: byteStream nextUint16Put: (representedObject basicAt: i) ].! !

!Indexed16BitForeignObject methodsFor: 'services' stamp: 'jmv 6/29/2023 16:45:38'!
referencedObjectsDo: aBlock
	"Evaluate aBlock for each object our representedObject points to.
	We don't reference any other objects."! !

!Indexed16BitForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 6/7/2023 17:35:46'!
canRepresent: anObject
	"We can work with indexable objects with 16 bit numeric slots."

	Smalltalk isSpur ifFalse: [ self halt ].
	^anObject class instSpec = 11! !

!Indexed32BitForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:35:36'!
slotCount
	"Answer the number of slots used by our representedObject"

	^representedObject basicSize! !

!Indexed32BitForeignObject methodsFor: 'accessing' stamp: 'jmv 5/23/2023 09:49:52'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Our slots are int32 or uint32."

	^4! !

!Indexed32BitForeignObject methodsFor: 'accessing' stamp: 'jmv 8/1/2023 15:39:30'!
spurObjectFormatFor: aSpurImageBuilder
	"Answer the format 5 bit field for Object Header in Spur.
	Use 0 or 1 low bits of size. Answer is 10 for Spur32 and in #(10 11) for Spur64."

	| slotCount oddBits slotsPerWord |
	slotCount := self slotCount.
	slotsPerWord := aSpurImageBuilder wordSize // 4.
	oddBits := ((slotCount / slotsPerWord) ceiling * slotsPerWord) - slotCount.
	^10 bitOr: oddBits! !

!Indexed32BitForeignObject methodsFor: 'serialization' stamp: 'jmv 8/17/2023 17:15:39'!
writeSlotsOn: byteStream forSpur: anImageBuilder

	"Actual data"
	1 to: self slotCount do: [ :i |
		anImageBuilder on: byteStream nextUint32Put: (representedObject basicAt: i) ].! !

!Indexed32BitForeignObject methodsFor: 'services' stamp: 'jmv 6/29/2023 16:45:45'!
referencedObjectsDo: aBlock
	"Evaluate aBlock for each object our representedObject points to.
	We don't reference any other objects."! !

!Indexed32BitForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 6/23/2023 17:09:27'!
canRepresent: anObject
	"We can work with indexable objects with 32 bit numeric slots, like WordArray, Bitmap, Float32Array, etc.
	Also for BoxedFloats, but they are handled in a special way. See #foreignObjectClassFor:"

	anObject isFloat ifTrue: [ ^false ].
	Smalltalk isSpur ifFalse: [ self halt ].
	^anObject class instSpec = 10! !

!Indexed64BitForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:35:40'!
slotCount
	"Answer the number of slots used by our representedObject"

	^representedObject basicSize! !

!Indexed64BitForeignObject methodsFor: 'accessing' stamp: 'jmv 5/23/2023 09:49:56'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Our slots are int64 or uint64."

	^8! !

!Indexed64BitForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:24:43'!
spurObjectFormat
	"Answer the format 5 bit field for Object Header in Spur."

	^9! !

!Indexed64BitForeignObject methodsFor: 'serialization' stamp: 'jmv 8/17/2023 17:15:55'!
writeSlotsOn: byteStream forSpur: anImageBuilder

	"Actual data"
	1 to: self slotCount do: [ :i |
		anImageBuilder on: byteStream nextUint64Put: (representedObject basicAt: i) ].! !

!Indexed64BitForeignObject methodsFor: 'services' stamp: 'jmv 6/29/2023 16:45:51'!
referencedObjectsDo: aBlock
	"Evaluate aBlock for each object our representedObject points to.
	We don't reference any other objects."! !

!Indexed64BitForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 6/7/2023 17:35:56'!
canRepresent: anObject
	"We can work with indexable objects with 64 bit numeric slots."

	Smalltalk isSpur ifFalse: [ self halt ].
	^anObject class instSpec = 9! !

!Indexed8BitForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 12:05:33'!
slotCount
	"Answer the number of slots used by our representedObject"

	^representedObject basicSize! !

!Indexed8BitForeignObject methodsFor: 'accessing' stamp: 'jmv 5/23/2023 09:50:01'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Our slots are int8 or uint8."

	^1! !

!Indexed8BitForeignObject methodsFor: 'accessing' stamp: 'jmv 8/1/2023 15:38:43'!
spurObjectFormatFor: aSpurImageBuilder
	"Answer the format 5 bit field for Object Header in Spur.
	Use 2 or 3 low bits of size. Answer in #(16 17 18 19) for Spur32 and in #(16 17 18 19 20 21 22 23) for Spur64."

	| oddBits slotCount slotsPerWord |
	slotCount := self slotCount.
	slotsPerWord := aSpurImageBuilder wordSize.
	oddBits := ((slotCount / slotsPerWord) ceiling * slotsPerWord) - slotCount.
	^16 bitOr: oddBits! !

!Indexed8BitForeignObject methodsFor: 'comparing' stamp: 'jmv 8/17/2023 17:30:37'!
identityHash
	"MethodDictionaries are created using the ForeignObjects as keys. See #addMethodDictionaryFor:
	When MethodDictionaries are later serialized, keys (so far, instances of this class) will be replaced by
	instances of Symbol, that will use this very identityHash value.
	Answer it as our #identityHash, so MethodDictionaries are created using the correct key hash values."

	^identityHash! !

!Indexed8BitForeignObject methodsFor: 'serialization' stamp: 'jmv 8/17/2023 17:16:10'!
writeSlotsOn: byteStream forSpur: anImageBuilder

	"Actual data"
	1 to: self slotCount do: [ :i |
		byteStream nextPut: (representedObject basicAt: i) ].! !

!Indexed8BitForeignObject methodsFor: 'services' stamp: 'jmv 6/29/2023 16:45:58'!
referencedObjectsDo: aBlock
	"Evaluate aBlock for each object our representedObject points to.
	We don't reference any other objects."! !

!Indexed8BitForeignObject methodsFor: 'printing' stamp: 'jmv 7/14/2023 09:21:36'!
printRepresentedObjectOn: aStream
	representedObject isString ifFalse: [
		^super printRepresentedObjectOn: aStream ].
	aStream nextPutAll: representedObject printString.! !

!Indexed8BitForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 6/7/2023 17:36:01'!
canRepresent: anObject
	"We can work with ByteArray and the like."

	Smalltalk isSpur ifFalse: [ self halt ].
	^anObject class instSpec = 16! !

!IndexedOopsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/20/2023 09:26:22'!
slotCount
	"Answer the number of slots used by our representedObject"

	^representedObject basicSize! !

!IndexedOopsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/19/2023 18:32:29'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Answer nil. Our slots are oops."

	^nil! !

!IndexedOopsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:12:11'!
spurObjectFormat
	"Answer the format 5 bit field for Object Header in Spur."

	^2! !

!IndexedOopsForeignObject methodsFor: 'serialization' stamp: 'jmv 8/17/2023 17:16:44'!
writeSlotsOn: byteStream forSpur: anImageBuilder

	representedObject do: [ :each | | fo |
		fo := (each is: #ForeignObject)
			ifTrue: [ each ]
			ifFalse: [ anImageBuilder findForeignObjectFor: each ].
		anImageBuilder on: byteStream nexWordPut: (fo foreignOopFor: anImageBuilder) ].! !

!IndexedOopsForeignObject methodsFor: 'services' stamp: 'jmv 6/29/2023 16:50:55'!
referencedObjectsDo: aBlock
	"Evaluate aBlock for each object our representedObject points to.
	To be redefined in classes whose representedObject actually holds OOP slots, like Arrays or object with instance variables."

	representedObject do: [ :each |
		aBlock value: each ].! !

!IndexedOopsForeignObject methodsFor: 'printing' stamp: 'jmv 7/14/2023 17:02:17'!
printRepresentedObjectOn: aStream! !

!IndexedOopsForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 6/7/2023 17:36:06'!
canRepresent: anObject
	"We can work with Array and the like."

	Smalltalk isSpur ifFalse: [ self halt ].
	^anObject class instSpec = 2! !

!IvarsAndIndexedOopsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:43:38'!
slotCount
	"Answer the number of slots used by our representedObject"

	^representedObject basicSize + representedObject class instSize! !

!IvarsAndIndexedOopsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:43:34'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Answer nil. Our slots are oops."

	^nil! !

!IvarsAndIndexedOopsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:12:40'!
spurObjectFormat
	"Answer the format 5 bit field for Object Header in Spur."

	^3! !

!IvarsAndIndexedOopsForeignObject methodsFor: 'serialization' stamp: 'jmv 8/17/2023 17:16:58'!
writeSlotsOn: byteStream forSpur: anImageBuilder

	1 to: self slotCount do: [ :i | | each fo |
		each := representedObject instVarAt: i.
		fo := (each is: #ForeignObject)
			ifTrue: [ each ]
			ifFalse: [ anImageBuilder findForeignObjectFor: each ].
		anImageBuilder on: byteStream nexWordPut: (fo foreignOopFor: anImageBuilder) ].! !

!IvarsAndIndexedOopsForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 7/13/2023 15:11:34'!
canRepresent: anObject
	"We can work classes that are indexable and also include instance variables,
		like BlockClosure and AdditionalMethodState.
	But not MethodDictionary or MethodContext."

	anObject class == MethodDictionary ifTrue: [ ^false ].
	anObject class == MethodContext ifTrue: [ ^false ].
	Smalltalk isSpur ifFalse: [ self halt ].
	^anObject class instSpec = 3! !

!IvarsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 15:38:42'!
slotCount
	"Answer the number of slots used by our representedObject"

	^representedObject class instSize! !

!IvarsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 12:12:41'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Answer nil. Our slots are oops."

	^nil! !

!IvarsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:12:50'!
spurObjectFormat
	"Answer the format 5 bit field for Object Header in Spur."

	^1! !

!IvarsForeignObject methodsFor: 'serialization' stamp: 'jmv 8/17/2023 17:17:11'!
writeSlotsOn: byteStream forSpur: anImageBuilder

	1 to: self slotCount do: [ :i | | each fo |
		each := representedObject instVarAt: i.
		fo := (each is: #ForeignObject)
			ifTrue: [ each ]
			ifFalse: [ anImageBuilder findForeignObjectFor: each ].
		anImageBuilder on: byteStream nexWordPut: (fo foreignOopFor: anImageBuilder) ].! !

!IvarsForeignObject methodsFor: 'services' stamp: 'jmv 6/29/2023 16:50:14'!
referencedObjectsDo: aBlock
	"Evaluate aBlock for each object our representedObject points to.
	To be redefined in classes whose representedObject actually holds OOP slots, like Arrays or object with instance variables."

	1 to: self slotCount do: [ :i |
		aBlock value: (representedObject instVarAt: i) ].! !

!IvarsForeignObject methodsFor: 'printing' stamp: 'jmv 7/14/2023 17:01:51'!
printRepresentedObjectOn: aStream! !

!IvarsForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 6/7/2023 17:36:18'!
canRepresent: anObject
	"We can work with regular classes with instance variables, like Point and OrderedCollection.
	We don't work with classes or metaclasses."

	anObject isBehavior ifTrue: [ ^false ].
	(anObject is: #CodeElementSpec) ifTrue: [ ^false ].
	Smalltalk isSpur ifFalse: [ self halt ].
	^anObject class instSpec = 1! !

!LargeIntegerForeignObject methodsFor: 'accessing' stamp: 'jmv 8/14/2023 17:41:17'!
slotCount
	"Answer the number of slots used by our representedObject"

	^representedObject basicSize! !

!LargeIntegerForeignObject methodsFor: 'accessing' stamp: 'jmv 8/14/2023 17:40:49'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Our slots are int8 or uint8."

	^1! !

!LargeIntegerForeignObject methodsFor: 'accessing' stamp: 'jmv 8/14/2023 17:47:29'!
spurObjectFormatFor: aSpurImageBuilder
	"Answer the format 5 bit field for Object Header in Spur.
	Use 2 or 3 low bits of size. Answer in #(16 17 18 19) for Spur32 and in #(16 17 18 19 20 21 22 23) for Spur64."

	| oddBits slotCount slotsPerWord |
	slotCount := self slotCount.
	slotsPerWord := aSpurImageBuilder wordSize.
	oddBits := ((slotCount / slotsPerWord) ceiling * slotsPerWord) - slotCount.
	^16 bitOr: oddBits! !

!LargeIntegerForeignObject methodsFor: 'serialization' stamp: 'jmv 8/17/2023 17:17:25'!
writeSlotsOn: byteStream forSpur: anImageBuilder

	"Actual data"
	1 to: self slotCount do: [ :i |
		byteStream nextPut: (representedObject basicAt: i) ].! !

!LargeIntegerForeignObject methodsFor: 'services' stamp: 'jmv 8/14/2023 17:50:07'!
referencedObjectsDo: aBlock
	"Evaluate aBlock for each object our representedObject points to.
	We don't reference any other objects."! !

!MetaclassForeignObject methodsFor: 'accessing' stamp: 'jmv 6/2/2023 14:56:15'!
addMethod: aMethodForeignObject
	methodForeignObjects add: aMethodForeignObject ! !

!MetaclassForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 15:37:07'!
instanceVariablesForeignObject: aForeignObject
	instanceVariablesForeignObject := aForeignObject! !

!MetaclassForeignObject methodsFor: 'accessing' stamp: 'jmv 5/16/2023 15:33:58'!
methodDictForeignObject: aMethodDictionaryForeignObject
	methodDictForeignObject := aMethodDictionaryForeignObject! !

!MetaclassForeignObject methodsFor: 'accessing' stamp: 'jmv 6/7/2023 17:59:22'!
methodForeignObjects
	^methodForeignObjects! !

!MetaclassForeignObject methodsFor: 'accessing' stamp: 'jmv 6/23/2023 18:15:38'!
representedObject: aLocalObject oop: newOop identityHash: newIdentityHash
	super representedObject: aLocalObject oop: newOop identityHash: newIdentityHash.
	nameOfClassToInstantiate := #Metaclass! !

!MetaclassForeignObject methodsFor: 'accessing' stamp: 'jmv 5/23/2023 09:53:53'!
slotCount
	"Answer the number of slots used by our representedObject"

	"Assuming standard representation in Cuis / Squeak.
	Refector as needed when this no longer true, to reflect the source code for the image being built!!
	Metaclass allInstVarNames size = 6"
	^ 6! !

!MetaclassForeignObject methodsFor: 'accessing' stamp: 'jmv 5/20/2023 13:59:57'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Answer nil. Our slots are oops."

	^nil! !

!MetaclassForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:13:04'!
spurObjectFormat
	"Answer the format 5 bit field for Object Header in Spur."

	^1! !

!MetaclassForeignObject methodsFor: 'accessing' stamp: 'jmv 7/12/2023 12:39:03'!
thisClassForeignObject
	^thisClassForeignObject! !

!MetaclassForeignObject methodsFor: 'accessing' stamp: 'jmv 5/16/2023 15:11:23'!
thisClassForeignObject: aClassForeignObject
	thisClassForeignObject := aClassForeignObject! !

!MetaclassForeignObject methodsFor: 'initialization' stamp: 'jmv 6/2/2023 14:55:32'!
initialize

	super initialize.
	methodForeignObjects := OrderedCollection new.! !

!MetaclassForeignObject methodsFor: 'serialization' stamp: 'jmv 8/17/2023 17:18:32'!
writeSlotsOn: byteStream forSpur: anImageBuilder

	| superclassForeignObject format |
	
	"Superclass oop"
	superclassForeignObject := representedObject superclass
		ifNotNil: [ :superclass | (anImageBuilder findForeignObjectFor: superclass) metaclassForeignObject]
		ifNil: [ anImageBuilder findClassNamed: #Class ].
	anImageBuilder on: byteStream nexWordPut: (superclassForeignObject foreignOopFor: anImageBuilder).

	"methodDict oop"
	anImageBuilder on: byteStream nexWordPut: (methodDictForeignObject foreignOopFor: anImageBuilder).

	"format oop"
	 format := ClassBuilder new computeFormat: #normal fullInstancesSize: thisClassForeignObject slotCount.
"Only valid when exactly cloning classes, and running on Spur too"
self assert: (Smalltalk at: representedObject name) class format = format.
	anImageBuilder on: byteStream nexWordPut: (anImageBuilder foreignOopOfSmallInteger: format).

	"instanceVariables oop"
	anImageBuilder on: byteStream nexWordPut: (instanceVariablesForeignObject foreignOopFor: anImageBuilder).

	"organization oop"
	anImageBuilder on: byteStream nexWordPut: ((anImageBuilder findForeignObjectFor: nil) foreignOopFor: anImageBuilder).

	"thisClass oop"
	anImageBuilder on: byteStream nexWordPut: (thisClassForeignObject foreignOopFor: anImageBuilder).! !

!MetaclassForeignObject methodsFor: 'services' stamp: 'jmv 7/6/2023 10:12:08'!
localBindingOf: varNameSymbol for: anImageBuilder

	^thisClassForeignObject localBindingOf: varNameSymbol for: anImageBuilder! !

!MetaclassForeignObject methodsFor: 'printing' stamp: 'jmv 7/14/2023 09:20:14'!
printRepresentedObjectOn: aStream
	aStream nextPutAll: representedObject name.
	aStream nextPutAll: ' class'.! !

!MethodContextForeignObject methodsFor: 'accessing' stamp: 'jmv 8/17/2023 16:08:24'!
slotCount
	"Answer the number of slots used by our representedObject"

	^self contextFrameSize + representedObject class instSize! !

!MethodContextForeignObject methodsFor: 'accessing' stamp: 'jmv 7/11/2023 19:02:29'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Answer nil. Our slots are oops."

	^nil! !

!MethodContextForeignObject methodsFor: 'accessing' stamp: 'jmv 7/11/2023 19:02:33'!
spurObjectFormat
	"Answer the format 5 bit field for Object Header in Spur."

	^3! !

!MethodContextForeignObject methodsFor: 'serialization' stamp: 'jmv 8/18/2023 14:50:36'!
writeSlotsOn: byteStream forSpur: anImageBuilder

	| nilForeignObject |
	nilForeignObject := (anImageBuilder findForeignObjectFor: nil) foreignOopFor: anImageBuilder.
	"Instance variables"
	1 to: representedObject class instSize do: [ :i | | each fo |
		each := representedObject instVarAt: i.
		fo := (each is: #ForeignObject)
			ifTrue: [ each ]
			ifFalse: [ anImageBuilder findForeignObjectFor: each ].
		anImageBuilder on: byteStream nexWordPut: (fo foreignOopFor: anImageBuilder) ].
	"Frame pointers"
	1 to: representedObject basicSize do: [ :i | | each fo |
		each := representedObject basicAt: i.
		fo := (each is: #ForeignObject)
			ifTrue: [ each ]
			ifFalse: [ anImageBuilder findForeignObjectFor: each ].
		anImageBuilder on: byteStream nexWordPut: (fo foreignOopFor: anImageBuilder) ].
	"nil beyond knowable end of stack"
	self contextFrameSize - representedObject basicSize timesRepeat: [
		anImageBuilder on: byteStream nexWordPut: nilForeignObject ].! !

!MethodContextForeignObject methodsFor: 'private' stamp: 'jmv 7/13/2023 16:20:35'!
contextFrameSize
	"Valid in mother image. We'd need to get correct value for any seed image."

	^ representedObject method
		ifNotNil: [ :method | method frameSize]
		ifNil: [CompiledMethod fullFrameSize]! !

!MethodContextForeignObject methodsFor: 'compatibility' stamp: 'jmv 9/14/2023 15:44:03'!
isDone
	"This is done so our Process instance is not taken for runnable processes.
	See #isTerminated	"
	^true! !

!MethodContextForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 7/11/2023 19:02:12'!
canRepresent: anObject

	Smalltalk isSpur ifFalse: [ self halt ].
	anObject class == MethodContext ifTrue: [ ^true ].
	^false! !

!MethodDictionaryForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:43:10'!
slotCount
	"Answer the number of slots used by our representedObject"

	^representedObject basicSize + representedObject class instSize! !

!MethodDictionaryForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:38:03'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Answer nil. Our slots are oops."

	^nil! !

!MethodDictionaryForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:13:31'!
spurObjectFormat
	"Answer the format 5 bit field for Object Header in Spur."

	^3! !

!MethodDictionaryForeignObject methodsFor: 'serialization' stamp: 'jmv 8/17/2023 17:17:33'!
writeSlotsOn: byteStream forSpur: anImageBuilder

	1 to: self slotCount do: [ :i | | each fo |
		each := representedObject instVarAt: i.
		fo := (each is: #ForeignObject)
			ifTrue: [ each ]
			ifFalse: [ anImageBuilder findForeignObjectFor: each ].
		anImageBuilder on: byteStream nexWordPut: (fo foreignOopFor: anImageBuilder) ].! !

!MethodDictionaryForeignObject methodsFor: 'printing' stamp: 'jmv 7/14/2023 17:01:46'!
printRepresentedObjectOn: aStream! !

!MethodDictionaryForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 6/7/2023 17:35:10'!
canRepresent: anObject
	"We can represent instances of 	MethodDictionary."

	^anObject class == MethodDictionary! !

!Spur32FreeListsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/19/2023 18:04:15'!
slotCount
	"Answer the number of slots used by our representedObject"

	^32! !

!Spur32FreeListsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/23/2023 09:56:35'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Our slots are 32 bit integers, handled only by the VM to store private raw pointers."

	^4! !

!Spur32FreeListsForeignObject methodsFor: 'accessing' stamp: 'jmv 6/1/2023 16:34:27'!
spurClassIndexFor: aSpurImageBuilder
	"Not a regular Smalltalk object.
	See class comment."

	^ 18! !

!Spur32FreeListsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:21:37'!
spurObjectFormat
	"Answer the format 5 bit field for Object Header in Spur.
	Our size is fixed and even, we don't need to worry about the padding low bit and format 11."

	^10! !

!Spur32FreeListsForeignObject methodsFor: 'serialization' stamp: 'jmv 8/15/2023 18:36:59'!
writeSlotsOn: byteStream forSpur: anImageBuilder
	"Just a bunch of zeros. It is up to the VM to use this as desired."

	byteStream nextPutAll: (ByteArray new: self slotCount * self slotWidth).! !

!Spur64FreeListsForeignObject methodsFor: 'accessing' stamp: 'jmv 8/17/2023 16:07:34'!
slotCount
	"Answer the number of slots used by our representedObject"

	^64! !

!Spur64FreeListsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/23/2023 09:56:31'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Our slots are 64 bit integers, handled only by the VM to store private raw pointers."

	^8! !

!Spur64FreeListsForeignObject methodsFor: 'accessing' stamp: 'jmv 6/1/2023 16:34:34'!
spurClassIndexFor: aSpurImageBuilder
	"Not a regular Smalltalk object.
	See class comment."

	^ 19! !

!Spur64FreeListsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:21:48'!
spurObjectFormat
	"Answer the format 5 bit field for Object Header in Spur."

	^9! !

!Spur64FreeListsForeignObject methodsFor: 'serialization' stamp: 'jmv 8/15/2023 18:37:03'!
writeSlotsOn: byteStream forSpur: anImageBuilder
	"Just a bunch of zeros. It is up to the VM to use this as desired."

	byteStream nextPutAll: (ByteArray new: self slotCount * self slotWidth).! !

!SpurPseudoArrayForeignObject methodsFor: 'accessing' stamp: 'jmv 8/17/2023 16:08:00'!
slotCount
	"Answer the number of slots used by our representedObject.
	In the new image, we'll be a special object with an special class pointer"

	^representedObject basicSize! !

!SpurPseudoArrayForeignObject methodsFor: 'accessing' stamp: 'jmv 5/20/2023 09:26:58'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Answer nil. Our slots are oops."

	^nil! !

!SpurPseudoArrayForeignObject methodsFor: 'accessing' stamp: 'jmv 6/1/2023 16:35:01'!
spurClassIndexFor: aSpurImageBuilder
	"Not a regular Smalltalk object.
	See class comment."

	^ 16! !

!SpurPseudoArrayForeignObject methodsFor: 'accessing' stamp: 'jmv 5/23/2023 09:55:23'!
spurObjectFormat
	"Answer the format 5 bit field for Object Header in Spur."

	^2! !

!SpurPseudoArrayForeignObject methodsFor: 'serialization' stamp: 'jmv 8/17/2023 15:43:45'!
writeSlotsOn: byteStream forSpur: anImageBuilder

	representedObject do: [ :pageOrClassOrMetaclassForeignObjectOrNil | | fo |
		fo := pageOrClassOrMetaclassForeignObjectOrNil
			ifNil: [ anImageBuilder findForeignObjectFor: nil ].
		anImageBuilder on: byteStream nexWordPut: (fo foreignOopFor: anImageBuilder) ].! !

!SpurPseudoArrayForeignObject methodsFor: 'printing' stamp: 'jmv 7/14/2023 17:02:01'!
printRepresentedObjectOn: aStream! !

!WeakIndexedOopsForeignObject methodsFor: 'accessing' stamp: 'jmv 8/17/2023 16:08:14'!
slotCount
	"Answer the number of slots used by our representedObject"

	^representedObject basicSize! !

!WeakIndexedOopsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:37:10'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Answer nil. Our slots are oops."

	^nil! !

!WeakIndexedOopsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:22:52'!
spurObjectFormat
	"Answer the format 5 bit field for Object Header in Spur."

	^4! !

!WeakIndexedOopsForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 6/7/2023 17:36:26'!
canRepresent: anObject
	"We can work classes that are indexable and that reference weakely, like WeakArray."

	Smalltalk isSpur ifFalse: [ self halt ].
	^anObject class instSpec = 4! !

!WeakIvarsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:36:58'!
slotCount
	"Answer the number of slots used by our representedObject"

	^representedObject class instSize! !

!WeakIvarsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:37:05'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject.
	Answer nil. Our slots are oops."

	^nil! !

!WeakIvarsForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:23:09'!
spurObjectFormat
	"Answer the format 5 bit field for Object Header in Spur."

	^5! !

!WeakIvarsForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 6/7/2023 17:36:30'!
canRepresent: anObject
	"We can work classes that have ivars and that reference weakely, like Ephemeron."

	Smalltalk isSpur ifFalse: [ self halt ].
	^anObject class instSpec = 5! !

!ZeroSizeForeignObject methodsFor: 'accessing' stamp: 'jmv 5/19/2023 17:38:29'!
slotCount
	"Answer the number of slots used by our representedObject"

	^0! !

!ZeroSizeForeignObject methodsFor: 'accessing' stamp: 'jmv 5/19/2023 18:25:56'!
slotWidth
	"Answer the size of each slot, in bytes, used by our representedObject
	It is irrelevant, our #slotCount is zero."

	^nil! !

!ZeroSizeForeignObject methodsFor: 'accessing' stamp: 'jmv 5/22/2023 16:23:29'!
spurObjectFormat
	"Answer the format 5 bit field for Object Header in Spur."

	^0! !

!ZeroSizeForeignObject methodsFor: 'serialization' stamp: 'jmv 8/17/2023 17:18:10'!
writeSlotsOn: byteStream forSpur: anImageBuilder! !

!ZeroSizeForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 6/7/2023 17:36:36'!
canRepresent: anObject
	"We can work with nil, false, true and regular objects with no instance variables."

	Smalltalk isSpur ifFalse: [ self halt ].
	^anObject class instSpec = 0! !

!ImmediateForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 11:08:19'!
foreignOopFor: anImageBuilder
	"Oop (Object Pointer) to be used in the newly created image.
	Ask to Builder for it!!"

	^self subclassResponsibility! !

!ImmediateForeignObject methodsFor: 'accessing' stamp: 'jmv 6/23/2023 18:08:28'!
representedObject: aLocalObject

	self subclassResponsibility! !

!ImmediateForeignObject methodsFor: 'services' stamp: 'jmv 6/29/2023 16:46:14'!
referencedObjectsDo: aBlock
	"Evaluate aBlock for each object our representedObject points to.
	We don't reference any other objects."! !

!ImmediateForeignObject class methodsFor: 'instance creation' stamp: 'jmv 6/23/2023 17:38:23'!
newFor: aLocalObject
	"Answer an instance of the correct subclass."

	^self new representedObject: aLocalObject! !

!ImmediateForeignObject class methodsFor: 'testing' stamp: 'jmv 6/23/2023 17:34:20'!
instancesAreForImmediateObjects
	"Answer true if our instances represent objects that will not be allocated in a new image,
	because they will be immediate, and fully encoded in their OOP."

	^true! !

!ImmediateCharacterForeignObject methodsFor: 'accessing' stamp: 'jmv 8/15/2023 18:29:43'!
foreignIdentityHashFor: anImageBuilder
	"IdentityHash to be used in the newly created image.
	Depends, as a minimum, on the image format. Perhaps the builder can know.
	Is it possible that some VMs answer differently? I guess not, but where is that contract written?"

	^self error: 'must implement?'! !

!ImmediateCharacterForeignObject methodsFor: 'accessing' stamp: 'jmv 8/17/2023 14:37:45'!
foreignOopFor: anImageBuilder
	"The oop is computed by the builder, based on our represented object"

	^anImageBuilder foreignOopOfImmediateCharacter: representedObject.! !

!ImmediateCharacterForeignObject methodsFor: 'accessing' stamp: 'jmv 8/15/2023 18:29:43'!
representedObject: aLocalObject

	representedObject := aLocalObject.
	nameOfClassToInstantiate := #Character.! !

!ImmediateCharacterForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 8/15/2023 18:29:43'!
canRepresent: anObject
	"Characters are handled in a special way. See #foreignObjectClassFor:"

	^super canRepresent: anObject! !

!SmallFloat64ForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 10:53:38'!
foreignIdentityHashFor: anImageBuilder
	"IdentityHash to be used in the newly created image.
	Depends, as a minimum, on the image format. Perhaps the builder can know.
	Is it possible that some VMs answer differently? I guess not, but where is that contract written?"

	^self error: 'must implement?'! !

!SmallFloat64ForeignObject methodsFor: 'accessing' stamp: 'jmv 6/30/2023 18:13:47'!
foreignOopFor: anImageBuilder
	"The oop is computed by the builder, based on our represented object"

	^anImageBuilder foreignOopOfSmallFloat64: representedObject.! !

!SmallFloat64ForeignObject methodsFor: 'accessing' stamp: 'jmv 6/23/2023 18:07:59'!
representedObject: aLocalObject

	representedObject := aLocalObject.
	nameOfClassToInstantiate := #SmallFloat64.! !

!SmallFloat64ForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 6/23/2023 17:08:04'!
canRepresent: anObject
	"Floats are handled in a special way. See #foreignObjectClassFor:"

	^super canRepresent: anObject! !

!SmallIntegerForeignObject methodsFor: 'accessing' stamp: 'jmv 6/26/2023 11:06:01'!
foreignIdentityHashFor: anImageBuilder
	"IdentityHash to be used in the newly created image.
	SmallIntegers are always (?) their own identityHash."

	^representedObject! !

!SmallIntegerForeignObject methodsFor: 'accessing' stamp: 'jmv 6/29/2023 16:28:57'!
foreignOopFor: anImageBuilder
	"The oop is computed by the builder, based on our represented object"

	^anImageBuilder foreignOopOfSmallInteger: representedObject.! !

!SmallIntegerForeignObject methodsFor: 'accessing' stamp: 'jmv 6/23/2023 18:08:11'!
representedObject: aLocalObject

	representedObject := aLocalObject.
	nameOfClassToInstantiate := #SmallInteger.! !

!SmallIntegerForeignObject class methodsFor: 'subclass selection' stamp: 'jmv 6/23/2023 17:18:46'!
canRepresent: anObject
	"Integers are handled in a special way. See #foreignObjectClassFor:"

	^super canRepresent: anObject! !

!ProtoObject methodsFor: '*Bootstrap-newImageOverrides' stamp: 'jmv 9/20/2023 16:32:52'!
noclash01_become: otherObject 
	"Primitive. Swap the object pointers of the receiver and the argument.
	All variables in the entire system that used to point to the 
	receiver now point to the argument, and vice-versa.
	Fails if either object is a SmallInteger"

	{ self } elementsExchangeIdentityWith: { otherObject }! !

!Object methodsFor: '*Bootstrap-newImageOverrides' stamp: 'jmv 9/4/2023 19:19:36'!
noclash01_doesNotUnderstand: aMessage
	| context count |
	StdIOWriteStream stdout flush.
	StdIOWriteStream stdout newLine;
		nextPutAll: 'Message not understood.'; newLine;
		nextPutAll: 'Receiver: '.

	self class isMeta
		ifFalse: [ StdIOWriteStream stdout nextPutAll: 'a ' ]
		ifTrue: [ StdIOWriteStream stdout nextPutAll: 'class ' ].
	self class theNonMetaClass printNameOn: StdIOWriteStream stdout.

	StdIOWriteStream stdout
		nextPutAll: ' selector: #';
		nextPutAll: aMessage selector; newLine;
		nextPutAll: '------ Call Stack: '; newLine;
		flush.

	context := thisContext sender.
	count := 0.
	[context notNil and: [ count < 10 ]] whileTrue: [
		context printOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout newLine.
		context := context sender.
		count := count + 1 ].

	StdIOWriteStream stdout newLine;
		nextPutAll: 'Will exit Cuis now.';
		newLine;
		flush.

	Smalltalk quitPrimitive: 127.! !

!Object methodsFor: '*Bootstrap-newImageOverrides' stamp: 'jmv 9/5/2023 09:26:33'!
noclash01_error: aString

	| context count |
	StdIOWriteStream stdout newLine;
		nextPutAll: 'Error: ';
		nextPutAll: aString; newLine;
		nextPutAll: '------ Call Stack: '; newLine;
		flush.

	context := thisContext sender.
	count := 0.
	[context notNil and: [ count < 10 ]] whileTrue: [
		context printOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout newLine.
		context := context sender.
		count := count + 1 ].

	StdIOWriteStream stdout newLine;
		nextPutAll: 'Will exit Cuis now.';
		newLine;
		flush.
	Smalltalk quitPrimitive: 128.! !

!Object methodsFor: '*Bootstrap-newImageOverrides' stamp: 'jmv 9/4/2023 18:56:59'!
noclash01_printOn: aStream
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver.
	String streamContents: [ :strm | 	'amazing thing' noclash01_printOn: strm ]
	"

	aStream nextPutAll: 'a '.
	self class printNameOn: aStream.! !

!Object methodsFor: '*Bootstrap-newImageOverrides' stamp: 'jmv 8/31/2023 16:40:47'!
noclash01_printString
	"Answer a String whose characters are a description of the receiver. 
	If you want to print without a character limit, use fullPrintString.

	This description is to be meaningful for a Smalltalk programmer and usually includes
	a hint on the class of the object.

	Usually you should not reimplement this method in subclasses, but #printOn:

	See the comments at:
		#printString
		#displayStringOrText
		#asString
		#storeString"

	^String streamContents: [:s | self printOn: s]! !

!Object methodsFor: '*Bootstrap-newImageOverrides' stamp: 'jmv 9/18/2023 13:03:57'!
noclash01_warning: aString
"
7 noclash01_warning: 'El totón!!'
"

	StdIOWriteStream stdout
		nextPutAll: 'Warning: ';
		nextPutAll: aString; newLine;
		flush.! !

!Behavior methodsFor: '*Bootstrap-newImageOverrides' stamp: 'jmv 9/11/2023 11:13:25'!
noclash01_allInstances
	"Answer all instances of the receiver."
	<primitive: 177>! !

!Character class methodsFor: '*Bootstrap-newImageOverrides' stamp: 'jmv 8/2/2023 10:32:54'!
noclash01_numericValue: anInteger
	"Answer the Character whose value is anInteger."

	"Don't use Character Table. Only for Spur."

	^self immediateNumericValue: anInteger! !

!Symbol methodsFor: '*Bootstrap-newImageOverrides' stamp: 'jmv 8/2/2023 10:32:58'!
noclash01_storeOn: aStream 
	"To be used when we don't have Scanner (Parser, Compiler, etc...)"

	aStream nextPut: $#.
	aStream nextPutAll: self.! !

!SystemDictionary methodsFor: '*Bootstrap-examples' stamp: 'jmv 9/14/2023 10:38:28'!
bootstrappedExample01Stdout
	"A simple demo to be run in a bootstrapped Cuis image, with a minimal class library"

	StdIOWriteStream stdout
		nextPutAll: 'Hello, World!!'; newLine;
		nextPutAll: 'This is Bootstrapped Cuis!!'; newLine;
		nextPutAll: '3 + 4 is ', (3 + 4) printString; newLine;
		flush.

	self quitPrimitive: 42.! !

!SystemDictionary methodsFor: '*Bootstrap-examples' stamp: 'jmv 7/18/2023 11:54:15'!
bootstrappedExample02DoesNotUnderstand
	"A simple demo to be run in a bootstrapped Cuis image, with a minimal class library"

	StdIOWriteStream stdout
		nextPutAll: 'Hello, World!!'; newLine;
		nextPutAll: 'This is Bootstrapped Cuis!!'; newLine;
		nextPutAll: '3 + 4 is '; nextPutAll: (3 + 4) printString; newLine;
		flush.

	"To test handling of DNU"
	Smalltalk zork.

	self quitPrimitive: 42.! !

!SystemDictionary methodsFor: '*Bootstrap-examples' stamp: 'jmv 7/19/2023 16:06:12'!
bootstrappedExample03Float
	"A simple demo to be run in a bootstrapped Cuis image, with a minimal class library"
	StdIOWriteStream stdout
		nextPutAll: 'Hello, World!!'; newLine;
		nextPutAll: 'This is Bootstrapped Cuis!!'; newLine;
		nextPutAll: '3 + 4 is '; nextPutAll: (3 + 4) printString; newLine;
		flush.

	Float initialize.
	StdIOWriteStream stdout
		nextPutAll: 'Pi is approximately 355.0 / 113: '; nextPutAll: (355.0 / 113) printString; newLine;
		flush.

	self quitPrimitive: 42.! !

!SystemDictionary methodsFor: '*Bootstrap-examples' stamp: 'jmv 9/14/2023 10:46:54'!
bootstrappedExample04FileExperiments
	"A simple demo to be run in a bootstrapped Cuis image, with a minimal class library"

	| i s fileContents |
	StdIOWriteStream stdout
		nextPutAll: 'Hello, World!!'; newLine;
		nextPutAll: 'This is Bootstrapped Cuis!!'; newLine;
		nextPutAll: '3 + 4 is '; nextPutAll: (3 + 4) printString; newLine;
		nextPutAll: 'primImageName: '; nextPutAll: (Smalltalk primImageName); newLine;
		nextPutAll: 'primGetCurrentWorkingDirectoryUnix: '; nextPutAll: (Smalltalk 			primGetCurrentWorkingDirectoryUnix); newLine;
		nextPutAll: 'platformName: '; nextPutAll: (Smalltalk platformName); newLine;
		flush.
	i := 0.
	s := '---Built in Modules---'.
	[
		StdIOWriteStream stdout nextPutAll: s; newLine; flush.
		i := i + 1.
		s := Smalltalk listBuiltinModule: i.
		s == nil] whileFalse.
	StdIOWriteStream stdout nextPutAll: 'They were: '; nextPutAll: (i-1) printString; newLine; flush.

	i := 0.
	s := '---Loaded Modules---'.
	[
		StdIOWriteStream stdout nextPutAll: s; newLine; flush.
		i := i + 1.
		s := Smalltalk listLoadedModule: i.
		s == nil] whileFalse.
	StdIOWriteStream stdout nextPutAll: 'They were: '; nextPutAll: (i-1) printString; newLine; flush.
	StdIOWriteStream stdout
		nextPutAll: 'primVmFileName: '; nextPutAll: (Smalltalk primVmFileName); newLine;
		nextPutAll: 'primVmPath: '; nextPutAll: (Smalltalk primVmPath); newLine;
		flush.

	fileContents := BasicFileServices binaryContentsOf: 'comoVamos.txt'		.
	fileContents isNil
		ifTrue: [
			StdIOWriteStream stdout
				nextPutAll: 'File could not be read'; newLine;
				flush ]
		ifFalse: [
			StdIOWriteStream stdout
				nextPutAll: fileContents size printString; newLine.
			fileContents printOn: StdIOWriteStream stdout.
			1 to: fileContents size do: [ :ii |
				StdIOWriteStream stdout nextPutAll: ' '; nextPutAll: (fileContents at: ii) printString ].
			StdIOWriteStream stdout
				newLine;
				flush ].

	self quitPrimitive: 42.! !

!SystemDictionary methodsFor: '*Bootstrap-examples' stamp: 'jmv 9/12/2023 12:02:03'!
bootstrappedExample05SymbolExperiments
	"A simple demo to be run in a bootstrapped Cuis image, with a minimal class library"

	| i s msg sym |

	"Better do it during image build..."
	Symbol initialize. 

	StdIOWriteStream stdout
		nextPutAll: 'Hello, World!!'; newLine;
		nextPutAll: 'This is Bootstrapped Cuis!!'; newLine;
		flush.

	i := 0.
	[
		i := i + 1.
		s := i printString.
		(Symbol lookup: s) notNil ] whileTrue.
	s.
	msg := (Symbol lookup: s) isNil
		ifTrue: [ '* does not exist' ]
		ifFalse: [ '* exists' ].
	StdIOWriteStream stdout
		nextPutAll: 'Symbol Lookup'; newLine;
		nextPutAll: 'The String: *';
		nextPutAll: s;
		nextPutAll: msg;
		nextPutAll: ' as a Symbol'; newLine;
		nextPutAll: 'Will attempt to create it'; newLine;
		flush.

	sym := s asSymbol.
	StdIOWriteStream stdout
		nextPutAll: 'Result of Symbol creation attempt was: *'.
	sym printOn: StdIOWriteStream stdout.
	StdIOWriteStream stdout
		nextPutAll: '*'; newLine;
		flush.

	msg := (Symbol lookup: s) isNil
		ifTrue: [ '* does not exist' ]
		ifFalse: [ '* exists' ].
	StdIOWriteStream stdout
		nextPutAll: 'Symbol Lookup'; newLine;
		nextPutAll: 'The String: *';
		nextPutAll: s;
		nextPutAll: msg;
		nextPutAll: ' as a Symbol'; newLine;
		nextPutAll: 'Thats all, Folks!!'; newLine;
		flush.

	self quitPrimitive: 42.! !

!SystemDictionary methodsFor: '*Bootstrap-examples' stamp: 'jmv 9/14/2023 10:26:15'!
bootstrappedExample06LoadDynamicLibrary
	"A simple demo to be run in a bootstrapped Cuis image, with a minimal class library"

	| className |
	"Better do it during image build..."
	Symbol initialize. 
	
	StdIOWriteStream stdout
		nextPutAll: 'Hello, World!!'; newLine;
		nextPutAll: 'This is Bootstrapped Cuis!!'; newLine;
		nextPutAll: 'Lets try some dcl'; newLine; newLine;
		flush.

	className := #DclExample03.
	StdIOWriteStream stdout
		nextPutAll: 'Do we include the library? ';
		nextPutAll: ((Smalltalk includesKey: className) ifTrue: ['yes'] ifFalse: ['no']); newLine;
		nextPutAll: 'Lets load it'; newLine;
		flush.

	(DynamicCuisLibrary readFromFileNamed: 'DclExample03.dcl')
		canBeInstalled;
		installLibrary.

	StdIOWriteStream stdout
		nextPutAll: 'Do we include the library? ';
		nextPutAll: ((Smalltalk includesKey: className) ifTrue: ['yes!!!!!!'] ifFalse: ['no']); newLine;
		flush.

	StdIOWriteStream stdout
		nextPutAll: 'Lets instantiate and compute stuff: ';
		nextPutAll: ((Smalltalk at: #DclExample03) new combined) printString;
		newLine;
		nextPutAll: 'Thats all, Folks!!'; newLine;
		flush.

	self quitPrimitive: 42.! !

!SystemDictionary methodsFor: '*Bootstrap-examples' stamp: 'jmv 9/20/2023 17:21:38'!
bootstrappedExample07LoadDynamicLibrary
	"A simple demo to be run in a bootstrapped Cuis image, with a minimal class library"

	| l t1 t2 t3 t4 t5 t6 |
	"Better do it during image build..."
	Symbol initialize. 
	
	StdIOWriteStream stdout
		nextPutAll: 'Hello, World!!'; newLine;
		nextPutAll: 'This is Bootstrapped Cuis!!'; newLine;
		nextPutAll: 'Lets try some dcl'; newLine; newLine;
		flush.

	StdIOWriteStream stdout
		nextPutAll: '1-Do we include the libraries? ';
		nextPutAll: ((Smalltalk includesKey: #MorphXX) ifTrue: ['yes'] ifFalse: ['no']); newLine;
		nextPutAll: 'Lets load it'; newLine;
		flush.

	t1 := [l := DynamicCuisLibrary readFromFileNamed: 'DclExample07MorphXX.dcl'] timeToRun.
	t2 := [l canBeInstalled] timeToRun.
	t3 := [l installLibrary] timeToRun.

	StdIOWriteStream stdout
		nextPutAll: '2-Do we include fist library? ';
		nextPutAll: ((Smalltalk includesKey: #MorphXX) ifTrue: ['yes!!!!!!'] ifFalse: ['no']); newLine;
		flush.

	StdIOWriteStream stdout
		nextPutAll: 'times ', t1 printString, ' ', t2 printString, ' ', t3 printString;
		newLine;
		flush.

	t4 := [l := DynamicCuisLibrary readFromFileNamed: 'DclExample07MorphXX10.dcl'] timeToRun.
	t5 := [l canBeInstalled] timeToRun.
	t6 := [l installLibrary] timeToRun.

	StdIOWriteStream stdout
		nextPutAll: '3-Do we include second library? ';
		nextPutAll: ((Smalltalk includesKey: #MorphXX9) ifTrue: ['yes!!!!!!'] ifFalse: ['no']); newLine;
		flush.

	StdIOWriteStream stdout
		nextPutAll: 'times ', t4 printString, ' ', t5 printString, ' ', t6 printString;
		newLine;
		flush.

	StdIOWriteStream stdout
		nextPutAll: 'Thats all, Folks!!'; newLine;
		flush.

	self quitPrimitive: 42.! !

!DataStream class methodsFor: '*Bootstrap-newImageOverrides' stamp: 'jmv 9/5/2023 14:44:05'!
noclash01_initCachedState
	"TypeMap maps Smalltalk classes to type ID numbers which identify the data stream primitive formats.  nextPut: writes these IDs to the data stream.  NOTE: Changing these type ID numbers will invalidate all extant data stream files.  Adding new ones is OK.  
	Classes named here have special formats in the file.  If such a class has a subclass, it will use type 9 and write correctly.  It will just be slow.  (Later write the class name in the special format, then subclasses can use the type also.)
	 See nextPut:, next, typeIDFor:, & ReferenceStream>>isAReferenceType:"
	"
	DataStream noclash01_initCachedState
	"

	| t |
	t := TypeMap := Dictionary new: 80. "sparse for fast hashing"

	t at: UndefinedObject put: 1.
	t at: True put: 2.
	t at: False put: 3.
	t at: SmallInteger put: 4.
	t at: String put: 5.
	t at: Symbol put: 6.
	t at: ByteArray put: 7.
	t at: Array put: 8.
	t at: Bitmap put: 11.
	t at: Metaclass put: 12.
	t at: Float put: 14.
	t at: BoxedFloat64 put: 14.
	t at: SmallFloat64 put: 14.
	t at: String put: 17.
	"t at: WordArray put: 18."
	"t at: Float32Array put: 20."
	"For all classes that are like WordArrays, store them the way ColorArray is stored.  As bits, and able to change endianness."
"OJO"
"
	Smalltalk do: [:cls |
		cls isInMemory ifTrue: [
			cls isBehavior ifTrue: [
				cls isPointers not & cls isVariable & cls isWords ifTrue: [
					(t includesKey: cls) ifFalse: [t at: cls print put: 20]]]]].
"
	t at: CompiledMethod put: 21.
	t at: Character put: 22.
	"t at: UnicodeString put: 23.
	t at: UnicodeSymbol put: 24."
	ReferenceTypes :=#(0 0 0 0 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 0 1 1).! !

!StdIOWriteStream methodsFor: '*Bootstrap-newImageOverrides' stamp: 'jmv 8/2/2023 10:33:02'!
noclash01_primWrite: id from: stringOrByteArray startingAt: startIndex count: count
	"Write count bytes onto this file from the given string or byte array starting at the given index. Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>! !
