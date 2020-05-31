import math._
import scala.util._
import scala.io.StdIn._
import java.{util => ju}
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._


/**
 * Bring data on patient samples from the diagnosis machine to the laboratory with enough molecules to produce medicine!
 **/

class Sample(var sampleId: Int,var carriedBy:Int,var rank:Int,var expertiseGain:String,var healt :Int,var costs:Array[Integer]   ){



}


class User(var storage:Array[Int],var target:String){


}





object Player extends App {

      def connect(module: String, data: String, position: String): Unit = {
      println("CONNECT " + data)

  }
          def move(module: String, data: String, position: String): Unit = {
      println("GOTO " + module)

  }

    val projectCount = readLine.toInt
    for(i <- 0 until projectCount) {
        val Array(a, b, c, d, e) = (readLine split " ").map (_.toInt)
    }

    // game loop
    while(true) {
    var samples = new ListBuffer[Sample]() 
    var users   = new ListBuffer[User]()
    var my_storage : Array[Int] = null
        for(i <- 0 until 2) {
            val Array(target, _eta, _score, _storageA, _storageB, _storageC, _storageD, _storageE, _expertiseA, _expertiseB, _expertiseC, _expertiseD, _expertiseE) = readLine split " "
            val eta = _eta.toInt
            val score = _score.toInt
            val storageA = _storageA.toInt
            val storageB = _storageB.toInt
            val storageC = _storageC.toInt
            val storageD = _storageD.toInt
            val storageE = _storageE.toInt
            val expertiseA = _expertiseA.toInt
            val expertiseB = _expertiseB.toInt
            val expertiseC = _expertiseC.toInt
            val expertiseD = _expertiseD.toInt
            val expertiseE = _expertiseE.toInt
            var user = new User(Array(storageA, storageB, storageC, storageD, storageE),target)
            
            users.append(user)
                               System.err.println("user " + user)

            if(i == 0){
            my_storage = Array(storageA, storageB, storageC, storageD, storageE)
            }
        }
        val Array(availableA, availableB, availableC, availableD, availableE) = (readLine split " ").map (_.toInt)
        val sampleCount = readLine.toInt
        for(i <- 0 until sampleCount) {
            val Array(_sampleId, _carriedBy, _rank, expertiseGain, _health, _costA, _costB, _costC, _costD, _costE) = readLine split " "
            val sampleId = _sampleId.toInt
            val carriedBy = _carriedBy.toInt
            val rank = _rank.toInt
            val health = _health.toInt
            val costA = _costA.toInt
            val costB = _costB.toInt
            val costC = _costC.toInt
            val costD = _costD.toInt
            val costE = _costE.toInt
            var  sample = new Sample(sampleId,carriedBy,rank,expertiseGain,health,Array(costA,costB,costC,costD,costE)) 
            samples.append(sample)  

        }
      var maxSample: Sample = null
    var myUser :User = (users(0))
    var maxHealt: Int = 0
    for (mysample <- samples) {
        if(mysample.healt>maxHealt && mysample.carriedBy < 1){

            maxSample = mysample
            maxHealt = mysample.healt

            }
         }
                   System.err.println("deneme 1 degil ")

    if (maxSample.carriedBy != 0) {
                if ("DIAGNOSIS" == myUser.target) {
                connect("DIAGNOSIS", maxSample.sampleId.toString(), myUser.target)
                } else {
                move("DIAGNOSIS", maxSample.sampleId.toString(), myUser.target)
                }

    } else {
        var myMolecule = new String
        breakable{
            for( i <- 0 to 4){
                if(myUser.storage(i) < maxSample.costs(i)){
                    myMolecule = "ABCDE".charAt(i).toString()
                    break()

                }

            }
        }
        if(!myMolecule.isEmpty()){
                if ("MOLECULES" == myUser.target) {
                connect("MOLECULES",myMolecule, myUser.target)
                } else {
                move("MOLECULES",myMolecule, myUser.target)
                }
            

        }else{
                if ("LABORATORY" == myUser.target) {
                connect("LABORATORY",maxSample.sampleId.toString(),myUser.target)
                } else {
                move("LABORATORY",maxSample.sampleId.toString(),myUser.target)
                }

        }

    }
        
        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
    }
}
