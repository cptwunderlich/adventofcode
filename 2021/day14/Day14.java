import java.nio.file.*;
import java.io.IOException;
import java.util.stream.*;
import java.util.*;

// This is an ugly, hacked-up version to help me with a bug in
// the C++ version I couldn't find.

public class Day14 {

    public static void main(String[] args) throws IOException {
        List<String> input = Files.lines(Path.of("./input")).collect(Collectors.toList());
        String template = input.get(0);

        Map<String, Character> rules = new HashMap<>();
        for (int i = 2; i < input.size(); i++) {
            String s = input.get(i);
            String key = s.substring(0, 2);
            rules.put(s.substring(0, 2), s.charAt(s.length()-1));
        }

        System.out.println("Part 1: " + solve(template, rules, 10));
        System.out.println("Part 2: " + solve(template, rules, 40));
    }

    private static long solve(String template, Map<String, Character> rules, int iterations) {
        Map<String, Long> pairCounts = new HashMap<>();
        Map<Character, Long> letterCounts = new HashMap<>();
        for (int i=0; i < template.length()-1; i++) {
            pairCounts.merge(""+ template.charAt(i) + template.charAt(i+1), 1L, Long::sum);
            letterCounts.merge(template.charAt(i), 1L, Long::sum);
        }
        letterCounts.merge(template.charAt(template.length()-1), 1L, Long::sum);

        for (int i=0; i < iterations; i++) {
            Map<String, Long> tmp = new HashMap<>();
            for (Map.Entry<String, Long> e : pairCounts.entrySet()) {
                String key = e.getKey();
                char c = rules.get(key);
                long cnt = e.getValue();
                tmp.merge(""+ key.charAt(0) + c, cnt, Long::sum);
                tmp.merge(""+ c + key.charAt(1), cnt, Long::sum);
                letterCounts.merge(c, cnt, Long::sum);
            }

            pairCounts = tmp;
        }

        long min = Long.MAX_VALUE, max = 0;
        for (long val : letterCounts.values()) {
            if (val < min) min = val;
            if (val > max) max = val;
        }

        return max - min;
    }
}