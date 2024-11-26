package com.weizilla.adventofcode.y2023;

import com.weizilla.adventofcode.utils.Day;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;

public class Day07Part2 extends Day {
    private static class Hand implements Comparable<Hand> {
        private final NavigableMap<Integer, Integer> counter;
        private final List<Integer> values;
        private final String org;
        private Type type;
        private int bid;

        private Hand(String org) {
            this.org = org;
            values = new ArrayList<>();
            counter = new TreeMap<>();
        }

        public void add(Integer value) {
            counter.merge(value, 1, Integer::sum);
            values.add(value);
        }

        public NavigableMap<Integer, Integer> getCounter() {
            return counter;
        }

        public List<Integer> keys() {
            return new ArrayList<>(counter.reversed().keySet());
        }

        @Override
        public int compareTo(Hand o) {
            int result = Integer.compare(getTypeRank(), o.getTypeRank());
            if (result == 0) {
                int i = 0;
                while (result == 0 && i < 5) {
                    result = Integer.compare(values.get(i), o.values.get(i));
                    i++;
                }
            }
            return result;
        }

        public Type getType() {
            return type;
        }

        public void setType(Type type) {
            this.type = type;
        }

        public int getTypeRank() {
            return this.type.rank;
        }

        public int getBid() {
            return bid;
        }

        public void setBid(int bid) {
            this.bid = bid;
        }

        public boolean isMatchCounts(int...values) {
            List<Integer> counts = counter.values().stream().sorted().toList();
            if (counts.size() != values.length) {
                return false;
            }

            List<Integer> wanted = Arrays.stream(values).sorted().boxed().toList();

            return wanted.equals(counts);

        }

        public int getNumJoker() {
            return this.counter.getOrDefault(1, 0);
        }

        @Override
        public String toString() {
            return org + "[" + values + "] (" + type + ")";
        }
    }

    private static final Map<Character, Integer> FACE_CARDS = Map.of(
        'A', 14, 'K', 13, 'Q', 12, 'J', 1, 'T', 10
    );

    private static final List<Type> TYPES = List.of(
        new FiveKind(),
        new FourKind(),
        new FullHouse(),
        new ThreeKind(),
        new TwoPair(),
        new OnePair(),
        new HighCard()
    );

    public Day07Part2(Integer example) {
        super(example);
    }

    @Override
    public Object part2() {
        List<String> strings = reader.readStrings();

        List<Hand> hands = new ArrayList<>();

        for (String line : strings) {
            var splits = line.split(" ");
            Hand hand = parseHand(splits[0]);
            var bid = Integer.parseInt(splits[1]);
            hand.setBid(bid);
            hands.add(hand);
        }

        print(hands);

        Collections.sort(hands);

        int result = 0;
        for (int i = 0; i < hands.size(); i++) {
            print("{} {}", i, hands.get(i));
            result += (i + 1) * hands.get(i).bid;
        }

        return result;
    }

    private Hand parseHand(String input) {
        Hand hand = new Hand(input);
        for (char c : input.toCharArray()) {
            Integer faceValue = FACE_CARDS.get(c);
            int value = faceValue != null ? faceValue : Integer.parseInt(c + "");
            hand.add(value);
        }

        for (Type t : TYPES) {
            if (t.isType(hand) && (hand.getType() == null || hand.getType().rank < t.rank)) {
                hand.setType(t);
            }
        }

        if (hand.getType() == null) {
            throw new RuntimeException("No type found hand:" + hand);
        }

        return hand;
    }

    private abstract static class Type {
        private final int rank;

        private Type(int rank) {
            this.rank = rank;
        }

        public boolean isType(Hand hand) {
            return isTypeJoker(hand) || isTypeNoJoker(hand);
        }

        abstract boolean isTypeNoJoker(Hand hand);

        abstract boolean isTypeJoker(Hand hand);

        @Override
        public String toString() {
            return getClass().getSimpleName();
        }
    }

    private static class FiveKind extends Type {
        private FiveKind() {
            super(7);
        }

        @Override
        public boolean isTypeNoJoker(Hand hand) {
            return hand.isMatchCounts(5);
        }

        @Override
        boolean isTypeJoker(Hand hand) {
            int numJoker = hand.getNumJoker();
            return numJoker > 0 && hand.isMatchCounts(numJoker, 5 - numJoker);
        }
    }

    private static class FourKind extends Type {
        private FourKind() {
            super(6);
        }

        @Override
        public boolean isTypeNoJoker(Hand hand) {
            return hand.isMatchCounts(4, 1);
        }

        @Override
        boolean isTypeJoker(Hand hand) {
            // 3 joker
            if (hand.getNumJoker() == 3 && hand.isMatchCounts(1, 1, 3)) {
                return true;
            }

            // 2 joker
            if (hand.getNumJoker() == 2 && hand.isMatchCounts(2, 2, 1)) {
                return true;
            }

            // 1 joker
            if (hand.getNumJoker() == 1 && hand.isMatchCounts(1, 1, 3)) {
                return true;
            }

            return false;
        }
    }

    private static class FullHouse extends Type {
        private FullHouse() {
            super(5);
        }

        @Override
        public boolean isTypeNoJoker(Hand hand) {
            return hand.isMatchCounts(2, 3);
        }

        @Override
        boolean isTypeJoker(Hand hand) {
            return hand.getNumJoker() == 1 && (hand.isMatchCounts(1, 1, 3) || hand.isMatchCounts(1, 2, 2));
        }
    }

    private static class ThreeKind extends Type {
        private ThreeKind() {
            super(4);
        }

        @Override
        public boolean isTypeNoJoker(Hand hand) {
            return hand.isMatchCounts(3, 1, 1);
        }

        @Override
        boolean isTypeJoker(Hand hand) {
            return hand.getNumJoker() > 0 && hand.isMatchCounts(1, 1, 1, 2);
        }
    }

    private static class TwoPair extends Type {
        private TwoPair() {
            super(3);
        }

        @Override
        public boolean isTypeNoJoker(Hand hand) {
            return hand.isMatchCounts(2, 2, 1);
        }

        @Override
        boolean isTypeJoker(Hand hand) {
            return hand.getNumJoker() > 0 && hand.isMatchCounts(1, 1, 1, 2);
        }
    }

    private static class OnePair extends Type {
        private OnePair() {
            super(2);
        }

        @Override
        public boolean isTypeNoJoker(Hand hand) {
            return hand.isMatchCounts(1, 1, 1, 2);
        }

        @Override
        boolean isTypeJoker(Hand hand) {
            return hand.getNumJoker() > 0 && hand.isMatchCounts(1, 1, 1, 1, 1);
        }
    }

    private static class HighCard extends Type {
        private HighCard() {
            super(1);
        }

        @Override
        public boolean isTypeNoJoker(Hand hand) {
            return hand.isMatchCounts(1, 1, 1, 1, 1);
        }

        @Override
        boolean isTypeJoker(Hand hand) {
            return false;
        }
    }
}
