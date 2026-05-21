[
  | serverClass |
  StdIOWriteStream stdout nextPutAll: 'starting cuis-mcp'; newLine; flush.
  serverClass := Smalltalk classNamed: #McpServer.
  serverClass ifNil: [ Error signal: 'McpServer is not installed in the image' ].

  (serverClass class includesSelector: #startOn:) ifFalse: [
    serverClass class compile: 'startOn: aPort
  socket ifNotNil: [ socket stop ].
  current := self new.
  self refreshTools.
  socket := McpSocket newForServer: current.
  socket startOn: aPort.
  ^ current' classified: 'instance creation'.
  ].

  serverClass stop.
  serverClass startOn: 1470.
  StdIOWriteStream stdout nextPutAll: 'cuis-mcp listening on localhost:1470'; newLine; flush.
] on: Error do: [ :ex |
  StdIOWriteStream stdout nextPutAll: 'ERROR: '; nextPutAll: ex printString; newLine; flush.
  StdIOWriteStream stdout nextPutAll: 'SIGNALER: '; nextPutAll: ex signalerContext printString; newLine; flush.
  Smalltalk quitPrimitive: 1
].
