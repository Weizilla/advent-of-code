package com.weizilla.adventofcode.y2023;

import com.weizilla.adventofcode.utils.Day;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;

public class Day07 extends Day {
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

        @Override
        public String toString() {
            return org + "[" + values + "] (" + type + ")";
        }
    }

    private static final Map<Character, Integer> FACE_CARDS_1 = Map.of(
        'A', 14, 'K', 13, 'Q', 12, 'J', 11, 'T', 10
    );

    private static final Map<Character, Integer> FACE_CARDS_2 = Map.of(
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

    public Day07(Integer example) {
        super(example);
    }

    @Override
    public Object part1() {
        List<String> strings = reader.readStrings();

        List<Hand> hands = new ArrayList<>();

        for (String line : strings) {
            var splits = line.split(" ");
            Hand hand = parseHand1(splits[0]);
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

//    @Override
//    public Object part2() {
//        List<String> strings = reader.readStrings();
//
//        List<Hand> hands = new ArrayList<>();
//
//        for (String line : strings) {
//            var splits = line.split(" ");
//            Hand hand = parseHand2(splits[0]);
//            var bid = Integer.parseInt(splits[1]);
//            hand.setBid(bid);
//            hands.add(hand);
//        }
//
//        print(hands);
//
//        Collections.sort(hands);
//
//        int result = 0;
//        for (int i = 0; i < hands.size(); i++) {
//            print("{} {}", i, hands.get(i));
//            result += (i + 1) * hands.get(i).bid;
//        }
//
//        return result;
//    }

    private Hand parseHand1(String input) {
        Hand hand = new Hand(input);
        for (char c : input.toCharArray()) {
            Integer faceValue = FACE_CARDS_1.get(c);
            int value = faceValue != null ? faceValue : Integer.parseInt(c + "");
            hand.add(value);
        }

        for (Type t : TYPES) {
            if (t.isType(hand)) {
                hand.setType(t);
            }
        }

        return hand;
    }

    private Hand parseHand2(String input) {
        Hand hand = new Hand(input);
        for (char c : input.toCharArray()) {
            Integer faceValue = FACE_CARDS_1.get(c);
            int value = faceValue != null ? faceValue : Integer.parseInt(c + "");
            hand.add(value);
        }

        int maxTypeRank = 0;
        Type type = null;
        for (Type t : TYPES) {
            if (t.isType(hand)) {
                if (t.rank > maxTypeRank) {
                    maxTypeRank = t.rank;
                    type = t;
                }
            }
        }
        hand.setType(type);

        return hand;
    }

    private abstract static class Type {
        private final int rank;

        private Type(int rank) {
            this.rank = rank;
        }

        abstract boolean isType(Hand hand);

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
        public boolean isType(Hand hand) {
            return hand.counter.size() == 1;
        }
    }

    private static class FourKind extends Type {
        private FourKind() {
            super(6);
        }

        @Override
        public boolean isType(Hand hand) {
            if (hand.counter.size() != 2) {
                return false;
            }

            int count1 = hand.getCounter().get(hand.keys().get(0));
            int count2 = hand.getCounter().get(hand.keys().get(1));

            return Math.max(count1, count2) == 4 && Math.min(count1, count2) == 1;
        }
    }

    private static class FullHouse extends Type {
        private FullHouse() {
            super(5);
        }

        @Override
        public boolean isType(Hand hand) {
            if (hand.counter.size() != 2) {
                return false;
            }

            int count1 = hand.getCounter().get(hand.keys().get(0));
            int count2 = hand.getCounter().get(hand.keys().get(1));

            return Math.max(count1, count2) == 3 && Math.min(count1, count2) == 2;
        }
    }

    private static class ThreeKind extends Type {
        private ThreeKind() {
            super(4);
        }

        @Override
        public boolean isType(Hand hand) {
            if (hand.counter.size() != 3) {
                return false;
            }

            int count1 = hand.getCounter().get(hand.keys().get(0));
            int count2 = hand.getCounter().get(hand.keys().get(1));
            int count3 = hand.getCounter().get(hand.keys().get(2));

            return (count1 == 1 && count2 == 1 && count3 == 3)
                || (count1 == 1 && count2 == 3 && count3 == 1)
                || (count1 == 3 && count2 == 1 && count3 == 1);
        }
    }

    private static class TwoPair extends Type {
        private TwoPair() {
            super(3);
        }

        @Override
        public boolean isType(Hand hand) {
            if (hand.counter.size() != 3) {
                return false;
            }

            int count1 = hand.getCounter().get(hand.keys().get(0));
            int count2 = hand.getCounter().get(hand.keys().get(1));
            int count3 = hand.getCounter().get(hand.keys().get(2));

            return (count1 == 2 && count2 == 2 && count3 == 1)
                || (count1 == 2 && count2 == 1 && count3 == 2)
                || (count1 == 1 && count2 == 2 && count3 == 2);
        }
    }

    private static class OnePair extends Type {
        private OnePair() {
            super(2);
        }

        @Override
        public boolean isType(Hand hand) {
            if (hand.counter.size() != 4) {
                return false;
            }

            int count1 = hand.getCounter().get(hand.keys().get(0));
            int count2 = hand.getCounter().get(hand.keys().get(1));
            int count3 = hand.getCounter().get(hand.keys().get(2));
            int count4 = hand.getCounter().get(hand.keys().get(3));

            return (count1 == 1 && count2 == 1 && count3 == 1 && count4 == 2)
                || (count1 == 1 && count2 == 1 && count3 == 2 && count4 == 1)
                || (count1 == 1 && count2 == 2 && count3 == 1 && count4 == 1)
                || (count1 == 2 && count2 == 1 && count3 == 1 && count4 == 1);
        }
    }

    private static class HighCard extends Type {
        private HighCard() {
            super(1);
        }

        @Override
        public boolean isType(Hand hand) {
            return hand.counter.size() == 5;
        }
    }
}
