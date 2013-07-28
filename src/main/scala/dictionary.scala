package com.DouglasCodes.CryptogramScala.Dictionary
import com.DouglasCodes.CryptogramScala.UnitOfLanguage
import scala.collection.immutable.HashMap
import scala.io.Source

class Dictionary(val name: String, srcfile: String ){
  var words =
    { for (l <- Source.fromFile(srcfile).getLines())
      yield (l -> OrderedUniqueString(l).size ) }.toMap.groupBy(f = (a) => a._1.size  )

  // def update() =
  //   words =

  override def toString: String =
    name + ": " + words.foldLeft(0)( (a, b) => a + b._2.size) + " words."
}
// class Dictionary
//   attr_accessor :name, :words

//   def initialize(name, file)
//     @words = Array.new(28)
//     @name = name
//     word_array = file.is_a?(Array) ? file : read_file_words(file)
//     word_array.each { |e| add_word(e) }
//   end

//   def read_file_words(file)
//     (IO.readlines(file)).map(&:strip).map(&:upcase)
//   end

//   def add_word(w)
//     @words[w.length] ||= Hash.new
//     return if @words[w.length].has_key?(w)
//     @words[w.length].store(w, unique_ify(w).length)
//   end

//   def size
//     x = 0
//     @words.each { |column|
//       next if column == nil
//       x += column.length
//     }
//     x
//   end

//   def find_possible_matches(word)
//     return [] if @words[word.length] == nil
//     p = @words[word.length].dup
//     p.keep_if { |k, v|
//         k.length == word.length && v == word.u_length && word.pattern_value == pattern_create(k)
//         }
//     p.keys
//   end

//   def to_s
//     "#{@name} has #{size()} words."
//   end
//   alias :inspect :to_s

// end
