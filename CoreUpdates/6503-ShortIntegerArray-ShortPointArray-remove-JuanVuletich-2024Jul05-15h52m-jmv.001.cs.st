'From Cuis7.1 [latest update: #6502] on 5 July 2024 at 3:56:38 pm'!

!ArrayedCollection commentStamp: 'jmv 7/5/2024 15:56:07' prior: 0!
I am an abstract superclass for collections of elements with the same storage size. My subclasses typically allocate storage as fixed size blocks. 

Element access is by an integer index, from 1 to collection size. Usual accessors are #at: and #at:put:

They are declared as having indexable instances. See:
	#variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
	#variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
	#variableDoubleByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
	#variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
	#variableDoubleWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
	#weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:

This means that they don't have instance variables. They are what is usually called an Array: a contiguous area of memory, that stores ObjectPointers (i.e. references to any objects), Bytes or 16, 32 or 64 bit Words. Each class is free to use that storarge to encode other kinds of data. See, for example, Float32Array Float64Array or DoubleByteArray. Therefore the size in bytes of each element is not constrained, although it is fixed for each class.

Instance size is fixed at creation. They can not grow or shrink.

For these reasons, there are many classes in the system that have Array like acessors (#at: and #at:put: with integer indexes) but are not in the ArrayedCollection hierarchy. These include OrderedCollection, Interval, Text, etc.!

!classRemoval: #ShortIntegerArray stamp: 'jmv 7/5/2024 15:52:56'!
Smalltalk removeClassNamed: #ShortIntegerArray!

!classRemoval: #ShortPointArray stamp: 'jmv 7/5/2024 15:52:52'!
Smalltalk removeClassNamed: #ShortPointArray!

