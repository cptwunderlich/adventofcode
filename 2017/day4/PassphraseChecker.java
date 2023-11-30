import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

/**
 * http://adventofcode.com/2017/day/4
 *
 * Part1: Passphrases without duplicate words.
 * Part2: Passphrases without permutations.
 *
 * @author Benjamin Maurer (maurer.benjamin@gmail.com)
 * @since 04.12.2017
 */
public class PassphraseChecker {

  public static void main(String[] args) throws IOException {
    List<String> lines = Files.readAllLines(Paths.get("./input.txt"));
    
    Long res1 = lines.stream().map(s -> s.split(" ")).filter(w -> w.length == Arrays.stream(w).distinct().count()).count();
    
    Long res2 = lines.stream().map(s -> s.split(" ")).filter(w -> w.length == Arrays.stream(w)
          .map(str -> str.chars().sorted().collect(StringBuilder::new,
                                                   StringBuilder::appendCodePoint, 
                                                   StringBuilder::append).toString())
          .distinct()
          .count())
        .count();
    
    System.out.println("Part1: " + res1 + "\nPart2: " + res2);
  }
}
