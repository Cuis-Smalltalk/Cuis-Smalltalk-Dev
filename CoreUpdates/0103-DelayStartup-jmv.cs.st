'From Squeak3.7 of ''4 September 2004'' [latest update: #5989] on 8 December 2008 at 3:11:30 pm'!!SystemDictionary class methodsFor: 'initialization' stamp: 'jmv 12/8/2008 15:09'!initialize
	"SystemDictionary initialize"

	| oldList |
	oldList := StartUpList.
	StartUpList := OrderedCollection new.
	"These get processed from the top down..."
	#(		#Delay 		#DisplayScreen 		#Cursor 		#InputSensor 		#ProcessorScheduler 	"Starts low space watcher and bkground."		#FileDirectory 			"Enables file stack dump and opens sources."		#ShortIntegerArray 		#ShortRunArray 		#CrLfFileStream) 	
		do: [ :clsName | 	
			Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToStartUpList: cls]].
	oldList ifNotNil: [
		oldList do: [ :className | 			Smalltalk 				at: className				ifPresent: [:theClass | Smalltalk addToStartUpList: theClass]]].
	#(		#ImageSegment 		#OldPasteUpMorph) 			do:  [ :clsName | 	
			Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToStartUpList: cls]].			
	oldList := ShutDownList.
	ShutDownList := OrderedCollection new.
	"These get processed from the bottom up..."
	#(		#Delay 		#DisplayScreen 		#InputSensor 		#Form 		#OldPasteUpMorph 		#StrikeFont 		#Color 		#FileDirectory 		#SoundPlayer 		#ImageSegment) 	
		do: [ :clsName | 	
			Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToShutDownList: cls]].
	oldList ifNotNil: [		oldList reverseDo: [ :className | 			Smalltalk 				at: className
				ifPresent: [:theClass | Smalltalk addToShutDownList: theClass]]]! !SystemDictionary initialize!