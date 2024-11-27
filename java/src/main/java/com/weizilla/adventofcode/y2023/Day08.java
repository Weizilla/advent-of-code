package com.weizilla.adventofcode.y2023;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;
import com.weizilla.adventofcode.utils.Day;
import org.checkerframework.checker.units.qual.N;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Day08 extends Day {
    public Day08(Integer example) {
        super(example);
    }

    @Override
    public Object part1() {
        List<String> lines = reader.readStrings();
        List<Step> steps = parseSteps(lines.getFirst());
        print(steps);

        Map<String, Node> nodes = new HashMap<>();
        for (int i = 1; i < lines.size(); i++) {
            String line = lines.get(i);
            String[] input = parseLine(line);

            Node node = nodes.computeIfAbsent(input[0], Node::new);
            Node left = nodes.computeIfAbsent(input[1], Node::new);
            Node right = nodes.computeIfAbsent(input[2], Node::new);

            node.left = left;
            node.right = right;
        }

        int numSteps = 0;
        Node curr = nodes.get("AAA");

        while (!curr.value.equals("ZZZ")) {
            for (Step step : steps) {
                switch (step) {
                    case L -> curr = curr.left;
                    case R -> curr = curr.right;
                }
                numSteps++;
            }
        }

        return numSteps;
    }

    @Override
    public Object part2() {
        List<String> lines = reader.readStrings();
        List<Step> steps = parseSteps(lines.getFirst());
        print(steps);

        Map<String, Node> nodes = new HashMap<>();
        for (int i = 1; i < lines.size(); i++) {
            String line = lines.get(i);
            String[] input = parseLine(line);

            Node node = nodes.computeIfAbsent(input[0], Node::new);
            Node left = nodes.computeIfAbsent(input[1], Node::new);
            Node right = nodes.computeIfAbsent(input[2], Node::new);

            node.left = left;
            node.right = right;
        }

        List<Node> currNodes = getStartingNodes(nodes.values());

        print("num starting {}", currNodes.size());

        Set<Integer> allNumSteps = new HashSet<>();

        for (Node node : currNodes) {
            int numSteps = 0;
            while (!node.isEnd) {
                for (Step step : steps) {
                    switch (step) {
                        case L -> node = node.left;
                        case R -> node = node.right;
                    }
                    numSteps++;
                }
            }
            allNumSteps.add(numSteps);
        }

        printAlways(allNumSteps);

        Multiset<Long> maxPrimes = HashMultiset.create();

        for (int n : allNumSteps) {
            var nPrimes = findPrimeFactors(n);
            printAlways("{} factors {}", n, nPrimes);
            for (int value : nPrimes.elementSet()) {
                int count = Math.max(maxPrimes.count(value), nPrimes.count(value));
                maxPrimes.setCount((long) value, count);
            }
        }

        printAlways(maxPrimes);

        long answer = maxPrimes.stream().reduce(1L, (a, b) -> a * b);

        return answer;
    }

    private List<Node> getStartingNodes(Collection<Node> nodes) {
        return nodes.stream()
            .filter(Node::isStart)
            .toList();
    }

    private Multiset<Integer> findPrimeFactors(int input) {
        int curr = input;
        Multiset<Integer> primes = HashMultiset.create();

        for (int i = 2; i < Math.pow(curr, 0.5); i++) {
            if (curr % i == 0) {
                primes.add(i);
                curr = curr / i;
                i = 1;
            }
        }

        primes.add(curr);

        return primes;
    }

    private List<Step> parseSteps(String line) {
        return line.chars()
            .mapToObj(c -> Step.valueOf(((char) c) + ""))
            .toList();
    }

    private static String[] parseLine(String line) {
        String clean = line.replaceAll("[(),=]", "");
        return clean.split("\s+");
    }

    private enum Step { L, R}

    private static class Node {
        private final String value;
        private final boolean isStart;
        private final boolean isEnd;
        private Node left;
        private Node right;

        public Node(String value) {
            this.value = value;
            isStart = value.endsWith("A");
            isEnd = value.endsWith("Z");
        }

        public String getValue() {
            return value;
        }

        public Node getLeft() {
            return left;
        }

        public void setLeft(Node left) {
            this.left = left;
        }

        public Node getRight() {
            return right;
        }

        public void setRight(Node right) {
            this.right = right;
        }

        public boolean isStart() {
            return isStart;
        }

        public boolean isEnd() {
            return isEnd;
        }
    }
}
