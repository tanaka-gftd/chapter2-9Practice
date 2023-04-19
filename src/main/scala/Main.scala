object Match {

  //2-9 初級問題
  //最初と最後の文字が同じ英数字であるランダムな 5 文字のシーケンスを 1000 個コンソールに出力する
  def printRandomChars(): Unit = {
    for (i <- 1 to 1000) {
      val chars: Seq[Char] = new scala.util.Random(new java.security.SecureRandom()).alphanumeric.take(5).toList
      val result = chars match {
        case Seq(a, b, c, d, e) =>
          if (a == e) {
            //最初と最後の英数字が同じなら、そのままSeqを返す
            chars
          } else {
            //最初と最後の英数字が異なるなら、Seqの末尾を最初の文字に変更した新しいSeqを返す
            //(scalaは関数式言語なので、Seqの要素の値を更新しただけでも、返り値としてSeqが返却されます)
            chars updated(4, a)
          }
      }
      /*
      //N予備校での解答はこちら
      // 不要となる配列の末尾を_(ワイルドカード)とし、新たにSeqを構築する。
      val result = chars match {
        case Seq(a, b, c, d, _) => Seq(a, b, c, d, a)  //新たなSeqは先頭と末尾の要素を同じ(ここではa)にする
      }
      */
      println(result)
    }
  }


  //2-9 中級問題
  //再帰とパターンマッチを利用して、シーケンスの最後の値を取得する last メソッドを実装する
  //分からなかったので、解答を参考
  def last(seq: Seq[Int]): Int = {
    seq match {
      /*
        要素が一つのSeqをつくり、それとマッチさせ、returnで値(変数xに格納される)を返却させることで、
        再帰的呼び出しから抜けることができる。
      */
      case Seq(x) => return x  //scalaのmatch式は1行ずつ解釈され、マッチした時点で処理を終えるので、こちらのcaseを上にする
      case x::xs => last(xs);  //xに先頭の要素、xsに残りの要素が格納される → 再帰的呼び出しで繰り返す
    }
  }


  //2-9 上級問題
  //再帰とパターンマッチを利用して要素の並びをひっくり返す reverse メソッドを実装する
  //中級問題で学んだことを利用することで、実装できました。
  def reverse(seq: Seq[Int]): Seq[Int] = {
    seq match {
      /*
        切り出した先頭の要素を:+メソッドでxsの末尾に加える → 再帰的呼び出しで繰り返す。
        再帰的呼び出しを繰り返していくうちに、xsの要素数は減っていくが、
        xsが空のSeqになった段階で reverse(xs)すると、エラーになるので、
        その前に、case Seq() => return Seq() を実行することで、再帰的呼び出しを終えるようにする。

        追記：
        case Seq() => return Seq() の部分は、
        case Seq() => Seq() でもOK
        (scalaは関数式言語なので、return句がなくても値は返却されるので)
      */
      case Seq() => return Seq()  //空のSeqとマッチしたら、returnで空のSeqを返し、再帰的呼び出しを終える
      case x::xs => reverse(xs) :+ x
    }

    /*
    //N予備校での解答はこちら
    //Seqの要素が一つになった時点で、再帰的呼び出しを終えるようにしている
    seq match {
      case Seq(x) => Seq(x)  //return句はなしでOK
      case x :: xs => reverse(xs) :+ x
    }
    */
  }
}
