'From Cuis 5.0 [latest update: #4830] on 2 September 2021 at 8:52:39 am'!

!DisplayScreen methodsFor: 'accessing' stamp: 'jmv 9/2/2021 08:52:10'!
getMainCanvas
	"Return a Canvas that can be used to draw onto the receiver.
	Being the sole 'main' canvas means a priviledge: Can use VectorEnginePlugin if available."

	| answer |
	answer _ [ MorphicCanvas withVectorEnginePluginOnForm: self ]
				on: OutOfMemory
				do: [
					'Not enough memory to run VectorEngine. Using BitBltCanvas instead.' print.
					MorphicCanvas activeSubclass: BitBltCanvas.
					MorphicCanvas withVectorEnginePluginOnForm: self ].
	^answer! !

