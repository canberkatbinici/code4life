import math._
import scala.util._
import scala.io.StdIn._
import java.{util => ju}
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._


/**
 * Bring data on patient samples from the diagnosis machine to the laboratory with enough molecules to produce medicine!
 **/

//Sample class for create regular syntax data for samples 

class Sample(var sampleId: Int,var carriedBy:Int,var rank:Int,var expertiseGain:String,var healt :Int,var costs:Array[Integer]   ){



}


//this class uses to collect our users datas. Storege is contains which molecules carried and target is which module needed
class User(var storage:Array[Int],var target:String){


}





object Player extends App {

      
      // connect funtion for connect to modules
      def connect( data: String): Unit = {
      println("CONNECT " + data)

  }
            // move funtion for move to modules

          def move(module: String): Unit = {
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
            var user = new User(Array(storageA, storageB, storageC, storageD, storageE),target) // create user object, with stored molecules and target module
            
            users.append(user) // add users to ListBuffer

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
            var  sample = new Sample(sampleId,carriedBy,rank,expertiseGain,health,Array(costA,costB,costC,costD,costE)) //create sample object for all sample in the diagonasis module
            samples.append(sample)  // add all samples to samples ListBuffer

        }
    
     
    var maxSample: Sample = null // maxSample var for handle max healted sample
    var myUser :User = (users(0))       //always first user is us. So, we get our user from users ListBuffer
    var maxHealt: Int = 0    // maxHealt var for handle max health
          
          // iterates all samples in samples ListBuffer and find which is has max healt value
    for (mysample <- samples) {
        if(mysample.healt>maxHealt && mysample.carriedBy < 1){ //If new sample's health bigger than our sample and new sample is not carried

            maxSample = mysample // new sample is become our sample
            maxHealt = mysample.healt //and max health become our sample's max health

            }
         }

          
          // this if condition checks is sample carried or not. If carried by us carriedby value is 0 else you need to carry one of the sample
    if (maxSample.carriedBy != 0) {
                if ("DIAGNOSIS" == myUser.target) {
                connect(maxSample.sampleId.toString())
                } else {
                move("DIAGNOSIS")
                }

    } else {
        var myMolecule = new String
          
          // control the storage 
        breakable{ //breakable needs to define in scala to break for loop
              
            for( i <- 0 to 4){ // for loop returns five times because there are 5 molecules 
                  
                if(myUser.storage(i) < maxSample.costs(i)){ //to define which molecule is missing
                    myMolecule = "ABCDE".charAt(i).toString() //save mising molecule name temperory 
                    break() // break for loop when catch missing molecule

                } 

            }
        }
        if(!myMolecule.isEmpty()){ // if there is a missing molecule 
                if ("MOLECULES" == myUser.target) { //if our target module is where we are already
                connect(myMolecule) // we just connect to module to take molecule
                } else { //if we have missing molecule but we are not in molecules module we move to molecules module
                move("MOLECULES") 
                }
            

        }else{ //if there is no missing molecule move laboratory or  connect laboratory for trade sample
                if ("LABORATORY" == myUser.target) { 
                connect(maxSample.sampleId.toString()) // if we already on laboratory module connect to laboratory with sample id
                } else {
                move("LABORATORY")// if we are not on laboratory module firstly move to laboratory module
                }

        }

    }

    }
}
