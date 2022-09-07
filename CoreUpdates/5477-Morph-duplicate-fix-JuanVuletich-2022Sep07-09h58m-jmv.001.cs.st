'From Cuis 6.0 [latest update: #5476] on 7 September 2022 at 9:59:35 am'!

!Object methodsFor: 'copying' stamp: 'jmv 9/7/2022 09:58:22'!
postCopy
	"self is a shallow copy, subclasses should copy fields as necessary to complete the full copy.
	Also clear any ivars that should be initialized lazyly."

	^ self! !

!Object methodsFor: 'copying' stamp: 'jmv 9/7/2022 09:55:04'!
veryDeepCopy
	"Do a full copy of an object graph.
	Some classes might chose not to include some ivars.
	For example, aMorph owner is only included 'weakely'.
	See #storeDataOn:, and in particular Morph>>storeDataOn:
	
	This method is included for comatibility with other Smalltalks and for reference.
	Each class should decide how to copy its instances.
	For example, Morph>>copy.
	Implementing #copy appropriately, and just calling it is better style than calling #veryDeepCopy (except, maybe, in low level, system code)"

	^ Object unStream: (ReferenceStream streamedRepresentationOf: self)! !


!Morph methodsFor: 'fileIn/out' stamp: 'jmv 9/7/2022 09:56:50'!
storeDataOn: aDataStream
	"Let all Morphs be written out.  All owners are weak references.  They only go out if the owner is in the tree being written."
	| cntInstVars cntIndexedVars ti instVarNames |

	"block my owner unless he is written out by someone else"
	cntInstVars _ self class instSize.
	cntIndexedVars _ self basicSize.
	instVarNames _ self class allInstVarNames.
	ti _ 1.  
	((instVarNames at: ti) = 'owner') & (Morph superclass == Object) ifFalse: [
		self error: 'this method is out of date'].
	aDataStream
		beginInstance: self class
		size: cntInstVars + cntIndexedVars.
	1 to: cntInstVars do: [ :i |
		(instVarNames at: i) caseOf: {
				['owner']  					-> [aDataStream nextPutWeak: owner]. 	"owner only written if in our tree"
				['id']  					-> [ aDataStream nextPut: (id bitAnd: 255) ].  "Clear id, but keep flags."
				['privateDisplayBounds']					-> [ aDataStream nextPut: nil ].  "Lazy initialized."
			}
			otherwise: [ aDataStream nextPut: (self instVarAt: i)]].
	1 to: cntIndexedVars do: [ :i |
		aDataStream nextPut: (self basicAt: i)]! !

