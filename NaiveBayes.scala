package lin567_p1


class NaiveBayes {
  // Use these to compute P( Language )
  var docLanguageCounts = Map[Language,Double]().withDefaultValue(0D)
  var docCount = 0D
  var languageNgramCounts = Map[Language,Double]().withDefaultValue(0D)
  var languageUniNgramCounts = Map[Language,Double]().withDefaultValue(0D)

  // Use these to compute P( Word | Language )
  var languageWordCounts = Map[Tuple2[Language,String],Double]().withDefaultValue(0D)

  // This should increment counts so you can compute P( Language ) and P( Word | Language )
  def train( corpus:Set[Document] ) {
    // This loops over the set of documents, and provides variables for the document id as a String,
    // the document text as an Array[String], and the language as a Language
    corpus.foreach{ case Document( id, text, language ) =>
      docCount += 1;
      if(docLanguageCounts.contains(language)){
        var temp = docLanguageCounts(language) + 1;
        docLanguageCounts += (language -> temp)
      }
      else{
        docLanguageCounts += (language -> 1)
      }

      for(nGram <- text){
        // println(nGram)
        if(languageWordCounts.contains((language, nGram))){
          var temp = languageWordCounts((language, nGram)) + 1
          languageWordCounts += ((language, nGram) -> temp)
        }
        else{
          languageWordCounts += ((language, nGram) -> 1)
          if(languageUniNgramCounts.contains(language)){
            var temp = languageUniNgramCounts(language) + 1;
            languageUniNgramCounts += language -> temp;
          }
          else{
            languageUniNgramCounts += language -> 1;
          }
          
        }

        if(languageNgramCounts.contains(language)){
          var temp = languageNgramCounts(language) + 1
          languageNgramCounts += language -> temp
        }
        else{
          languageNgramCounts += language -> 1;
        }


      }

     
      // Implement me
    }
    println(languageWordCounts)
    
  }

  // Should compute P( word | language ). Implement with add-lambda smoothing.
  def p_wordGivenLg( word:String, language:Language, lambda:Double ) = {
    // IMPLEMENT ME
    var deno = languageNgramCounts(language) + lambda * (languageUniNgramCounts(language) + 1)
    if(languageWordCounts.contains(language, word)){
      (languageWordCounts((language, word)) + lambda) / deno
    }
    else{
      lambda/deno
    }
    
  }

  // Should compute P( Language )
  def p_Lg( language:Language ) = {
    // IMPLEMENT ME
    if(docCount != 0){
      docLanguageCounts(language) / docCount
    }
    else{
      0D
    }
    
  }


  // Should compute P( Word, Language )= P( Language )\prod_{Word in Document}P( Word | Language )
  def p_docAndLg( document:Array[String], language:Language, lambda:Double ) = {
    // IMPLEMENT ME
    var lg_Prob = scala.math.log(p_Lg(language));
    for(nGram <- document){
      lg_Prob += scala.math.log(p_wordGivenLg(nGram, language, lambda));
    }
    // println(lg_Prob)

    lg_Prob
  }


  // This function takes a document as a parameter, and returns the highest scoring language as a
  // Language object. 
  def mostLikelyLanguage( document:Array[String], lambda:Double ) = {
    // Loop over the possible languages (they should accessible in docLanguageCounts.keys), and find
    // the language with the highest P( Document, Language ) score
    var maxProb = -9999999999D;
    var maxProbLan = Language("cze");


    docLanguageCounts.foreach{ case (language, count) => 
      var curLanProb = p_docAndLg(document, language, lambda);
      if( curLanProb > maxProb){
        maxProb = curLanProb;
        maxProbLan = language;
      }
    }

    maxProbLan
  }


}

