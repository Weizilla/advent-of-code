package com.weizilla.adventofcode.y2023;

import com.weizilla.adventofcode.utils.Day;
import org.checkerframework.checker.units.qual.N;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

        int numSteps = 0;
        Node[] currNodes = getStartingNodes(nodes.values());

        print("num starting {}", currNodes.length);

        while (!allAtEnd(currNodes)) {
            for (Step step : steps) {
                moveNodes(currNodes, step);
                numSteps++;
            }
        }

        return numSteps;
    }

    private Node[] getStartingNodes(Collection<Node> nodes) {
        return nodes.stream()
            .filter(Node::isStart)
            .toArray(Node[]::new);
    }

    private boolean allAtEnd(Node[] nodes) {
        for (int i = 0; i < nodes.length; i++) {
            if (!nodes[i].isEnd) {
                return false;
            }
        }
        return true;
    }

    private void moveNodes(Node[] nodes, Step step) {
        for (int i = 0; i < nodes.length; i++) {
            switch (step) {
                case L -> nodes[i] = nodes[i].left;
                case R -> nodes[i] = nodes[i].right;
            }
        }
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
