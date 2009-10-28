Reynolds
========

An implementation of a [BERT](http://bert-rpc.org) encoder and decoder in Scala.
On a bear-skin rug. Wearing only a mustache.


For To Build
------------

    $ mvn package
    $ cd target
    $ echo "The moment you grab someone by the lapels, you're lost."


For To Use
----------

    import com.codahale.bert.BERT
    
    val person = BERT("Coda Hale", List(1, 2, 3))
    val data = person.toArray
    
    BERT.parse(data) match {
      case BERT(name, numbers) =>
        println(name + " can count: " + numbers)
    }


For To Distribute
-----------------

MIT license. See COPYING.txt. Don't you ever take off that stupid hat?


(h/t to Toby Sterrett for the name.)