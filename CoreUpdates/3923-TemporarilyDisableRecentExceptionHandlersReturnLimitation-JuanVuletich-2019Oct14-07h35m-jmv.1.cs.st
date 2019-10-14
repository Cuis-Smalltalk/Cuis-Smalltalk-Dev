'From Cuis 5.0 [latest update: #3922] on 14 October 2019 at 7:46:20 am'!

!Exception methodsFor: 'priv handling' stamp: 'jmv 10/14/2019 07:43:10'!
evaluateHandlerBlock: aBlock

	| handlerEx |
	handlerBlockNotCurtailed := false.
	^[
			| answer |
			answer _ [aBlock valueWithPossibleArgument: self] on: Exception do: [:ex | handlerEx _ ex.  ex pass].
			handlerBlockNotCurtailed _ true.
			answer
	] ifCurtailed:
		[
			(handlerBlockNotCurtailed not and: [handlerEx isNil or: [handlerEx handlerBlockNotCurtailed not]])
				ifTrue: [
					"self error: 'Exception handler blocks must not do non local returns'."
					"Please see
https://gator3294.hostgator.com:2096/cpsess8738249540/3rdparty/squirrelmail/src/read_body.php?account=0&mailbox=INBOX&passed_id=116522&startMessage=1
https://gator3294.hostgator.com:2096/cpsess8738249540/3rdparty/squirrelmail/src/read_body.php?account=0&mailbox=INBOX&passed_id=116533&startMessage=1
					Also see the rest of the tread in detail.
					This is work in progress.
					Currently (October 14, 2019) system behavior is unaffected, except for the following message to the Transcript. But the necessary code to detect the questionable method returns in exception handlers is kept, to aid in further development.
					"
					'It is advisable to avoid method returns (non local returns) in exception handler blocks' print.
					]
				ifFalse: [handlerBlockNotCurtailed _ true]
		]! !

