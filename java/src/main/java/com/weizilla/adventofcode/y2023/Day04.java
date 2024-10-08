package com.weizilla.adventofcode.y2023;

import com.google.common.collect.Sets;
import com.weizilla.adventofcode.utils.Day;
import com.weizilla.adventofcode.utils.InputReader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class Day04 extends Day {
    private record Card(int id, Set<Integer> winning, Set<Integer> own) {}

    @Override
    public Object part1() {
        var lines = InputReader.readStrings();
        var cards = lines.stream().map(Day04::getCard).toList();

        var s = cards.stream()
            .mapToInt(c -> Sets.intersection(c.winning, c.own).size())
            .map(n -> (int) Math.pow(2, n - 1))
            .sum();

        return s;
    }

    @Override
    public Object part2() {
        var lines = InputReader.readStrings();
        var cards = lines.stream().map(Day04::getCard).toList();
        var wons = wons(cards);

        var totalCards = 0;
        var memo = new HashMap<Integer, Integer>();
        for (int i = 0; i < cards.size(); i++) {
            var numCards = numCards(i, wons, memo);
            totalCards += numCards;
        }

        return totalCards;
    }

    private static int numCards(int cardId, int[] numWinning, Map<Integer, Integer> memo) {
        if (cardId >= numWinning.length) {
            return 0;
        }

        if (memo.containsKey(cardId)) {
            return memo.get(cardId);
        }

        int totalCards = 1;
        int totalWon = numWinning[cardId];
        for (int i = 1; i <= totalWon; i++) {
            totalCards += numCards(cardId + i, numWinning, memo);
        }

        memo.put(cardId, totalCards);
        return totalCards;
    }

    private static Card getCard(String line) {
        var split = line.split(":");
        var id = Integer.parseInt(split[0].split(" +")[1]);
        var nums = split[1].split("[|]");
        var winning = Arrays.stream(nums[0].trim().split(" +"))
            .map(Integer::parseInt)
            .collect(Collectors.toSet());
        var own = Arrays.stream(nums[1].trim().split(" +"))
            .map(Integer::parseInt)
            .collect(Collectors.toSet());

        return new Card(id, winning, own);
    }

    private int[] wons(List<Card> cards) {
        var result = new int[cards.size()];
        for (int i = 0; i < cards.size(); i++) {
            Card card = cards.get(i);
            result[i] = Sets.intersection(card.winning, card.own).size();
        }
        return result;
    }
}
