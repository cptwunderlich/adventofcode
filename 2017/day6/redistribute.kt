import java.io.File

fun main(args: Array<String>) {
    val infile = File("./input.txt")
    val banks = infile.readLines()[0].split("\t").map{ s -> s.toInt() }.toMutableList()
    //val banks = mutableListOf(0, 2, 7, 0)
    val seen: MutableMap<Long, MutableList<Pair<Int, List<Int>>>> = HashMap()
    var steps = -1
    var blocks = 0
    var i = 0

    println(banks.toString())

    while (true) {
        if (blocks == 0) {
            steps++
            val h = hash(banks)
            println("Hash: $h, banks: ${banks.toString()}")
            if (seen.containsKey(h)) {
                val seenH = seen[h] ?: throw IllegalStateException("")
                for (l in seenH) {
                    println(l.toString())
                    if (l.second == banks) {
                        println("Res: $steps, cycles: ${steps - l.first}")
                        return
                    }
                }
                seen[h]!!.add(steps to banks)                
            } else {
                seen.put(h, mutableListOf(steps to banks))
            }
            blocks = banks.max() ?: 0
            i = banks.indexOf(blocks)
            println("Max: $blocks, i: $i")
            banks[i++] = 0
        } else {
            while (blocks > 0) {
                if (i >= banks.size) i = 0
                banks[i++]++
                blocks--
            }
        }
    }
}

fun hash(cfg: List<Int>) : Long {
    var res: Long = 0
    for (i in 0..cfg.size-1) {
        res += cfg[i] * Math.pow(10.0, i.toDouble()).toLong()
    }

    return res
}
