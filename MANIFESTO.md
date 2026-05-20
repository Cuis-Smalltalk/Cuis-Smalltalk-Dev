# Manifiesto: Principios de Disenio Detras de Smalltalk

Traduccion automatica al castellano (en progreso) del articulo:

- **Titulo original**: *Design Principles Behind Smalltalk*
- **Autor**: Daniel H. H. Ingalls, Learning Research Group, Xerox Palo Alto Research Center
- **Publicacion**: BYTE Magazine, Agosto de 1981. (c) The McGraw-Hill Companies, Inc., NY.
- **Fuente**: Copiado de http://users.ipa.net/~dwighth/smalltalk/byte_aug81/design_principles_behind_smalltalk.html
- **Digitalizacion**: Escaneado y convertido a HTML (con graficos recreados) por Dwight Hughes.

---

El proposito del proyecto Smalltalk es brindar soporte computacional al espiritu creativo presente en cada persona. Nuestro trabajo surge de una vision que incluye a un individuo creativo y al mejor hardware de computo disponible. Elegimos concentrarnos en dos areas principales de investigacion: un lenguaje de descripcion (lenguaje de programacion) que sirva de interfaz entre los modelos en la mente humana y los del hardware de computo, y un lenguaje de interaccion (interfaz de usuario) que haga corresponder el sistema de comunicacion humano con el de la computadora.

Nuestro trabajo ha seguido un ciclo de dos a cuatro anios que puede verse en paralelo con el metodo cientifico:

1. Construir un programa aplicativo dentro del sistema actual (hacer una observacion).
2. En base a esa experiencia, redisenar el lenguaje (formular una teoria).
3. Construir un nuevo sistema basado en el nuevo disenio (hacer una prediccion que pueda ponerse a prueba).

El sistema Smalltalk-80 marca nuestra quinta vuelta por este ciclo. En este articulo presento algunos de los principios generales que hemos observado a lo largo de nuestro trabajo. Si bien la presentacion suele rozar la "maternidad" de Smalltalk, los principios en si son mas generales y deberian resultar utiles al evaluar otros sistemas y al guiar trabajo futuro.

Para entrar en calor, empiezo con un principio mas social que tecnico, y que es en gran medida responsable del sesgo particular del proyecto Smalltalk:

## Maestria Personal

**Principio**: Si un sistema debe servir al espiritu creativo, debe ser completamente comprensible para un solo individuo.

El punto es que el potencial humano se manifiesta en individuos. Para realizar ese potencial debemos ofrecer un medio que pueda ser dominado por una sola persona. Cualquier barrera entre el usuario y alguna parte del sistema terminara siendo una barrera para la expresion creativa. Cualquier parte del sistema que no pueda cambiarse, o que no sea lo suficientemente general, es una fuente probable de obstaculos. Si una parte del sistema funciona distinto a todo el resto, esa parte requerira esfuerzo adicional para controlarla. Esa carga extra puede degradar el resultado final e inhibira emprendimientos futuros en esa area.

De esto inferimos un principio general de disenio:

## Buen Disenio

**Principio**: Un sistema debe construirse con un conjunto minimo de partes inmutables; esas partes deben ser lo mas generales posible; y todas las partes del sistema deben sostenerse dentro de un marco uniforme.

---

# Lenguaje

Al disenianar un lenguaje para usar con computadoras, no hace falta buscar lejos para encontrar pistas utiles. Todo lo que sabemos sobre como piensan y se comunican las personas aplica. Los mecanismos del pensamiento y de la comunicacion humanos fueron "ingenierizados" durante millones de anios, y deberiamos respetarlos como un disenio sano. Ademas, como debemos convivir con este disenio durante el proximo millon de anios, ahorraremos tiempo si hacemos nuestros modelos computacionales compatibles con la mente, y no al reves.

(La figura 1 del articulo original ilustra los componentes principales de esta discusion.) Una persona puede verse como cuerpo y mente. El cuerpo es el sitio de la experiencia primaria y, en este contexto, es el canal fisico por el cual el universo se percibe y por el cual las intenciones se llevan a cabo. La experiencia se registra y procesa en la mente. El pensamiento creativo (sin entrar en su mecanismo) puede verse como la aparicion espontanea de informacion en la mente. El lenguaje es la clave de esa informacion:

## Proposito del Lenguaje

**Principio**: Proveer un marco para la comunicacion.

La interaccion entre dos individuos puede pensarse en dos niveles. La comunicacion explicita son las palabras y movimientos efectivamente emitidos y percibidos. La comunicacion implicita es la cultura y experiencia compartidas que forman el contexto de la comunicacion explicita. En la interaccion humana, mucha comunicacion real se logra por referencia a contexto compartido, y el lenguaje humano esta construido en torno a esas alusiones. Con computadoras pasa lo mismo.

No es coincidencia que una computadora pueda verse como uno de los participantes de ese esquema. En ese caso, el "cuerpo" provee despliegue visual de informacion y sensado de entrada desde un usuario humano. La "mente" de la computadora incluye memoria interna, elementos de procesamiento y sus contenidos. De esto se desprende que hay varios temas involucrados en el disenio de un lenguaje de computo:

## Alcance

**Principio**: El disenio de un lenguaje para usar computadoras debe tratar modelos internos, medios externos y la interaccion entre ambos tanto en el humano como en la computadora.

Este hecho explica por que es dificil explicar Smalltalk a quienes ven los lenguajes de computacion en un sentido mas restringido. Smalltalk no es simplemente una mejor forma de organizar procedimientos o una tecnica distinta de gestion de almacenamiento. No es solo una jerarquia extensible de tipos, ni una interfaz grafica. Es todo eso y cualquier otra cosa necesaria para soportar las interacciones mencionadas.

---

# Objetos que se Comunican

La mente observa un vasto universo de experiencia, tanto inmediata como registrada. Uno puede derivar una sensacion de unidad con el universo simplemente dejando que esa experiencia sea, tal como es. Pero si uno desea participar, literalmente tomar parte, debe trazar distinciones. Al hacerlo identifica un objeto en el universo, y simultaneamente todo lo demas pasa a ser no-ese-objeto.

La distincion por si sola es un comienzo, pero distinguir no se vuelve mas facil. Cada vez que queres hablar de "esa silla de alla", tendrias que repetir todo el proceso de distinguir esa silla. Ahi aparece el acto de referir: asociamos un identificador unico a un objeto y, desde ese momento, basta mencionar ese identificador para referir al objeto original.

Hemos dicho que un sistema computacional debe proveer modelos compatibles con los de la mente. Por lo tanto:

## Objetos

**Principio**: Un lenguaje de computacion debe soportar el concepto de "objeto" y proveer un medio uniforme para referirse a los objetos de su universo.

El gestor de almacenamiento de Smalltalk provee un modelo orientado a objetos de memoria para todo el sistema. La referencia uniforme se logra asociando un entero unico a cada objeto del sistema. Esta uniformidad es importante porque significa que las variables pueden tomar valores muy distintos y aun asi implementarse como simples celdas de memoria. Los objetos se crean al evaluarse expresiones y pueden circular por referencia uniforme, de modo que los procedimientos que los manipulan no necesitan ocuparse de su almacenamiento. Cuando todas las referencias a un objeto desaparecen del sistema, el objeto se desvanece y su memoria se recupera. Ese comportamiento es esencial para sostener plenamente la metafora de objetos:

## Gestion de Almacenamiento

**Principio**: Para ser verdaderamente "orientado a objetos", un sistema debe proveer gestion automatica de almacenamiento.

Una forma de saber si un lenguaje esta funcionando bien es ver si los programas parecen hacer lo que hacen. Si estan salpicados con sentencias de gestion de memoria, el modelo interno no esta bien alineado con el humano. ¿Te imaginas tener que "preparar" a alguien antes de cada cosa que le decis, o informarle cuando ya terminaste con un tema para que lo olvide?

Cada objeto en nuestro universo tiene una vida propia. De modo similar, el cerebro provee procesamiento independiente junto con almacenamiento de cada objeto mental. Esto sugiere un tercer principio:

## Mensajes

**Principio**: El computo debe verse como una capacidad intrinseca de los objetos, invocable uniformemente mediante envio de mensajes.

Asi como los programas se ensucian si el almacenamiento se trata explicitamente, el control se complica si el procesamiento se realiza extrinsecamente. Consideremos sumar 5 a un numero. En muchos sistemas, el compilador decide que tipo de numero es y genera codigo para sumarle 5. Eso no alcanza en un sistema orientado a objetos, porque el tipo exacto no puede determinarse por el compilador (mas sobre esto despues). Una alternativa es llamar a una rutina general de suma que inspeccione los tipos para aproximar que hacer. Eso tampoco es bueno: obliga a que una rutina critica sea editada por novatos que solo quieren experimentar con su propia clase de numeros, y es mal disenio porque el conocimiento intimo de los internos de los objetos queda desparramado por el sistema.

Smalltalk da una solucion mas limpia: envia el nombre de la operacion deseada, junto con sus argumentos, como un mensaje al numero, con la idea de que el receptor sabe mejor como llevar a cabo la operacion. En vez de un procesador de bajo nivel "saqueando" estructuras de datos, tenemos un universo de objetos bien educados que se piden cortesmente entre si que realicen sus deseos. La transmision de mensajes es el unico proceso realizado fuera de los objetos, y asi debe ser, porque los mensajes viajan entre objetos.

El principio de buen disenio puede reformularse para lenguajes:

## Metafora Uniforme

**Principio**: Un lenguaje debe disenarse alrededor de una metafora poderosa que pueda aplicarse uniformemente en todas las areas.

Ejemplos exitosos: LISP, construido sobre el modelo de estructuras enlazadas; APL, sobre el modelo de arreglos; Smalltalk, sobre el modelo de objetos que se comunican. En cada caso, las aplicaciones grandes se ven de la misma manera que las unidades fundamentales de las que el sistema esta hecho. En Smalltalk, en particular, la interaccion entre los objetos mas primitivos se ve igual que la interaccion de mas alto nivel entre la computadora y el usuario. Cada objeto, incluso un humilde entero, tiene un conjunto de mensajes (un protocolo) que define la comunicacion explicita a la que puede responder. Internamente, los objetos pueden tener almacenamiento local y acceso a informacion compartida, que forman el contexto implicito de toda comunicacion. Por ejemplo, el mensaje `+ 5` (sumar cinco) lleva la suposicion implicita de que el sumando es el valor actual del numero que recibe el mensaje.

---

# Organizacion

Una metafora uniforme provee un marco para construir sistemas complejos. Varios principios organizacionales relacionados ayudan a manejar la complejidad. Para empezar:

## Modularidad

**Principio**: Ningun componente en un sistema complejo debe depender de los detalles internos de otro componente.

(La figura 2 del articulo original muestra que, si hay N componentes, hay aproximadamente N al cuadrado dependencias potenciales.) Si los sistemas de computo van a asistir en tareas humanas complejas, deben minimizar esa interdependencia. La metafora de envio de mensajes provee modularidad al desacoplar la intencion de un mensaje (encarnada en su nombre) del metodo que usa el receptor para llevarla a cabo. La informacion estructural tambien queda protegida porque todo acceso al estado interno de un objeto ocurre a traves de la misma interfaz de mensajes.

La complejidad suele reducirse agrupando componentes similares. En lenguajes convencionales se logra via tipos; en Smalltalk, via clases. Una clase describe otros objetos: su estado interno, el protocolo de mensajes que reconocen y los metodos internos para responder a esos mensajes. Los objetos asi descritos son instancias de esa clase. Incluso las clases encajan en el marco: son instancias de la clase `Class`, que describe el protocolo e implementacion apropiados para la descripcion de objetos.

## Clasificacion

**Principio**: Un lenguaje debe proveer un medio para clasificar objetos similares y para agregar nuevas clases de objetos en igualdad de condiciones con las clases nucleo del sistema.

La clasificacion es la objetificacion de la "-idad". Cuando alguien ve una silla, la experiencia se toma a la vez literalmente como "esa cosa" y abstractamente como "esa cosa tipo silla". Esa abstraccion surge de la capacidad de la mente de fusionar experiencias "similares", y se manifiesta como otro objeto mental: la silla platonica o la "silleidad".

Las clases son el mecanismo principal de extension en Smalltalk. Por ejemplo, un sistema de musica se crearia agregando clases que describan la representacion y el protocolo de interaccion de `Note`, `Melody`, `Score`, `Timbre`, `Player`, etc. La clausula de "igualdad de condiciones" es importante porque asegura que el sistema se use como fue disenado. Una melodia podria representarse como una coleccion ad hoc de enteros para altura, duracion y otros parametros; pero si el lenguaje maneja `Note` tan facilmente como enteros, el usuario tendera naturalmente a describir una melodia como coleccion de notas. En cada etapa de disenio, una persona elegira la representacion mas efectiva si el sistema la permite.

El principio de modularidad tiene una implicancia interesante para los componentes procedurales:

## Polimorfismo

**Principio**: Un programa debe especificar solo el comportamiento de los objetos, no su representacion.

Una formulacion convencional es que un programa no deberia declarar que un objeto dado es `SmallInteger` o `LargeInteger`, sino solo que responde al protocolo de enteros. Esta descripcion generica es crucial para modelar el mundo real.

Consideremos una simulacion de trafico. Muchos procedimientos se referiran a los distintos vehiculos. Supongamos que queremos agregar una barredora de calles. Si el codigo dependiera de los objetos que manipula, una extension simple implicaria recompilaciones y posibles errores. La interfaz de mensajes establece un marco ideal para extender: si la barredora soporta el mismo protocolo que los demas vehiculos, no hace falta cambiar nada para incluirla.

## Factorizacion

**Principio**: Cada componente independiente de un sistema debe aparecer en un solo lugar.

Hay varias razones. Primero, ahorra tiempo, esfuerzo y espacio si las adiciones deben hacerse en un solo lugar. Segundo, los usuarios localizan mas facilmente un componente que satisface una necesidad. Tercero, sin factorizacion adecuada aparecen problemas para sincronizar cambios y asegurar consistencia entre componentes interdependientes. Una falla de factorizacion equivale a violar modularidad.

Smalltalk fomenta disenios bien factorizados mediante herencia. Cada clase hereda comportamiento de su superclase. Esta herencia se extiende por clases cada vez mas generales hasta llegar a `Object`, que describe el comportamiento por defecto de todos los objetos. En la simulacion de trafico, `StreetSweeper` (y las demas clases de vehiculos) seria subclase de una clase general `Vehicle`, heredando comportamiento por defecto y evitando repetir conceptos en muchos lugares.

La herencia ilustra otro beneficio pragmatico de la factorizacion:

## Apalancamiento

**Principio**: Cuando un sistema esta bien factorizado, hay gran apalancamiento para usuarios e implementadores.

Pensemos en ordenar una coleccion ordenada. En Smalltalk, el usuario definiria un mensaje `sort` en la clase `OrderedCollection`. Hecho eso, todas las formas de colecciones ordenadas adquieren instantaneamente esa capacidad via herencia. El mismo metodo puede alfabetizar texto o ordenar numeros, ya que el protocolo de comparacion lo reconocen las clases que soportan texto y numeros.

Los beneficios estructurales para implementadores son claros. Habra menos primitivas que implementar. Por ejemplo, toda la grafica de Smalltalk se realiza con una sola operacion primitiva. Con una unica tarea, el implementador puede poner atencion a cada instruccion, sabiendo que cada mejora de eficiencia se amplificara en todo el sistema.

Es natural preguntar que conjunto de operaciones primitivas alcanza para soportar un sistema completo. A la respuesta se la llama especificacion de maquina virtual:

## Maquina Virtual

**Principio**: Una especificacion de maquina virtual establece un marco para la aplicacion de tecnologia.

La maquina virtual de Smalltalk establece un modelo orientado a objetos para almacenamiento, un modelo orientado a mensajes para procesamiento y un modelo de mapa de bits para la presentacion visual. Con microcodigo y, en ultima instancia, hardware, el rendimiento puede mejorar dramaticamente sin comprometer las otras virtudes del sistema.

---

# Interfaz de Usuario

Una interfaz de usuario es un lenguaje en el que la mayor parte de la comunicacion es visual. Como la presentacion visual se superpone fuertemente con la cultura humana, la estetica cumple un rol importante. Dado que toda capacidad del sistema se entrega finalmente a traves de la interfaz, la flexibilidad es esencial. Una condicion habilitante para una interfaz flexible puede expresarse como principio orientado a objetos:

## Principio Reactivo

**Principio**: Todo componente accesible al usuario debe poder presentarse de manera significativa para observacion y manipulacion.

Este criterio se sostiene bien en el modelo de objetos que se comunican. Por definicion, cada objeto provee un protocolo adecuado de mensajes para interaccion: un microlenguaje propio de ese tipo de objeto. En el nivel de interfaz, el lenguaje apropiado para cada objeto en pantalla se presenta visualmente (texto, menus, imagenes) y se percibe via teclado y un dispositivo apuntador.

Vale notar que los sistemas operativos suelen violar este principio. El programador debe salir de un marco consistente de descripcion, abandonar el contexto construido y tratar con un entorno distinto y normalmente muy primitivo. No deberia ser asi:

## Sistema Operativo

**Principio**: Un sistema operativo es una coleccion de cosas que no encajan en un lenguaje. No deberia haber uno.

Ejemplos de componentes tradicionales de un SO incorporados naturalmente al lenguaje Smalltalk:

1. **Gestion de almacenamiento**: completamente automatica. Los objetos se crean con un mensaje a su clase y se reclaman cuando no quedan referencias. La ampliacion del espacio de direcciones via memoria virtual es transparente.
2. **Sistema de archivos**: integrado como objetos `Files` y `Directories` con protocolos para acceso.
3. **Manejo de display**: el display es una instancia de `Form`, continuamente visible, y los mensajes de manipulacion grafica definidos en esa clase cambian la imagen visible.
4. **Entrada de teclado**: los dispositivos de entrada se modelan como objetos con mensajes para consultar estado o leer historial como secuencia de eventos.
5. **Acceso a subsistemas**: se incorporan como objetos independientes dentro de Smalltalk; pueden apoyarse en el universo de descripcion existente y, si interactuan con el usuario, participar como componentes de la UI.
6. **Debugger**: el estado del procesador Smalltalk es accesible como instancia de `Process` que posee una cadena de marcos de pila. El debugger es un subsistema Smalltalk con acceso para manipular el estado de un proceso suspendido. Casi el unico error de ejecucion posible en Smalltalk es que un mensaje no sea reconocido por su receptor.

Smalltalk no tiene un "sistema operativo" como tal. Las operaciones primitivas necesarias (por ejemplo leer una pagina de disco) se incorporan como metodos primitivos en respuesta a mensajes Smalltalk por lo demas normales.

---

# Trabajo Futuro

Como era de esperar, queda trabajo por hacer. Lo mas facil de describir es seguir aplicando los principios de este texto. Por ejemplo, Smalltalk-80 se queda corto en factorizacion porque soporta solo herencia jerarquica. Sistemas futuros generalizaran este modelo a herencia arbitraria (multiple). Ademas, los protocolos de mensajes no se han formalizado. La organizacion permite protocolos, pero hoy es mas una cuestion de estilo que sean consistentes entre clases. Esto puede remediarse proporcionando objetos de protocolo adecuados, compartibles consistentemente. Eso permitiria tipado formal de variables por protocolo sin perder las ventajas del polimorfismo.

El trabajo restante es menos facil de articular. Hay claramente otros aspectos del pensamiento humano que no se abordaron. Deben identificarse como metaforas que complementen los modelos existentes del lenguaje.

A veces el avance de los sistemas de computo parece deprimentemente lento. Olvidamos que las maquinas de vapor eran alta tecnologia para nuestros abuelos. Soy optimista: los sistemas de computo se estan volviendo mas simples y, como resultado, mas usables. Cierro con un principio general que gobierna este proceso:

## Seleccion Natural

**Principio**: Los lenguajes y sistemas de disenio sano persistiran, para ser reemplazados solo por otros mejores.

Mientras el reloj avanza, evoluciona soporte computacional cada vez mejor para el espiritu creativo. La ayuda esta en camino.
