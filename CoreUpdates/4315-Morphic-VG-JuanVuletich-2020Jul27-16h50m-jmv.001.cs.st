'From Cuis 5.0 [latest update: #4309] on 27 July 2020 at 5:10:43 pm'!

!Morph methodsFor: 'fileIn/out' stamp: 'jmv 7/27/2020 17:07:00'!
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
				['owner']  		-> [aDataStream nextPutWeak: owner]. 	"owner only written if in our tree"
				['id']  		-> [ aDataStream nextPut: nil ].
			}
			otherwise: [ aDataStream nextPut: (self instVarAt: i)]].
	1 to: cntIndexedVars do: [ :i |
		aDataStream nextPut: (self basicAt: i)]! !

