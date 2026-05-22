Class {
	#name : 'WorldTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Morphic-Kernel',
	#package : 'BaseImageTests',
	#tag : 'Morphic-Kernel'
}

{ #category : 'tests' }
WorldTest >> testDoOneCycleWorksWithDeferredQueue [
        "Ensure that nested doOneCycles don't break deferred UI messages"
        | finished |
        [
		[
	                UISupervisor whenUIinSafeState: [ UISupervisor ui doOneCycleNowOn: UISupervisor ui mainCanvas ].
	                UISupervisor whenUIinSafeState: nil "whatever".
			UISupervisor ui doOneCycleNowOn: UISupervisor ui mainCanvas.
	                finished := true.
	        ] valueWithin: 1 seconds onTimeout: [finished := false ].
	        self assert: finished
	] forkAt: Processor userInterruptPriority.
]
