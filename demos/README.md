# Demo library

Seven scenarios, each a real session from `data/` whose shape makes one
execution choice genuinely better than another. Every number below was
measured, not asserted — reproduce the lot with

```sh
dune exec bin/demo.exe          # all seven
dune exec bin/demo.exe -- 4     # just one
```

Nothing here is rigged. Each case names the market condition that drives
it, and each comparison changes exactly one thing: the algorithm, or one
parameter, or one deadline. Several cases end with the "obvious" answer
losing, which is the point — an execution desk's job is choosing per
situation, not finding a universally superior algorithm.

## How to read the tables

| column | meaning |
| --- | --- |
| `filled` | share of the requested quantity that executed |
| `shortfall` | average fill vs arrival price, side-adjusted bps — **positive is worse** |
| `vs vwap` | average fill vs the session's own VWAP, side-adjusted bps |
| `timing $` | the drift half of friction: what the market's own move cost while we worked |
| `impact c/sh` | cents per share we moved the price against ourselves |
| `vs immed $` | net P&L minus the immediate baseline's — positive means the algorithm beat it |

Impact per share is capped at **8.0¢** in every run: Engine A charges
`25¢ × √participation` and the participation cap is 10% of a bar, so
`25 × √0.10 ≈ 7.9¢` is the model's ceiling. A config sitting at 8.0¢ is
one that traded as hard as the simulator allows.

---

## 1. VWAP tracks a U-shaped day

**Shows:** shaping a schedule to the volume curve tracks the VWAP
benchmark more closely than spreading evenly.

```csv
timestamp,symbol,side,quantity,deadline
09:31:00,META,BUY,400000,15:59:00
```

**Market condition — META 2026-07-10.** The most U-shaped session in the
catalog: 40.4% of the day's 26.7M shares trade in the first and last
half hour, and the ratio of edge volume to midday volume is 3.8×.

**Compare:** `vwap` · `twap` · `immediate`, all defaults.

| config | filled | shortfall | vs vwap | impact c/sh |
| --- | --- | --- | --- | --- |
| vwap | 100.0% | 86.7 | **−3.2** | 3.64 |
| twap | 100.0% | 82.4 | −7.4 | 4.01 |
| immediate | 16.8% | 1.5 | −87.6 | 8.00 |

**Why.** TWAP puts the same number of shares into a quiet 13:00 minute
as into a heavy 09:35 one, so it spends the midday trading where the
crowd is not. VWAP's schedule follows the forecast curve and lands
nearer the benchmark.

**The metric that reveals it: `vs vwap`, read as distance from zero.**
VWAP's mandate is to *track* the day's VWAP, not to beat it. Both
algorithms happen to beat it here (both negative) because the tape drifted
their way — but VWAP is 3.2bps from the benchmark against TWAP's 7.4bps.
Judging VWAP by shortfall instead would credit it for drift it never
tried to control.

**Note the immediate row:** 400,000 shares is 16.8% of what the arrival
minute can absorb, so immediacy simply cannot finish, and pays the
ceiling impact on what it does get.

---

## 2. A flat day punishes VWAP's forecast

**Shows:** the same reasoning that wins case 1 loses when today does not
look like the average day. This is case 1's honest counterweight.

```csv
timestamp,symbol,side,quantity,deadline
09:31:00,META,BUY,250000,15:59:00
```

**Market condition — META 2026-07-17.** The *flattest* profile in the
catalog: edge volume is only 0.7× the midday, against 3.8× on 07-10.

**Compare:** `twap` · `vwap`, all defaults.

| config | filled | shortfall | vs vwap |
| --- | --- | --- | --- |
| twap | 100.0% | −65.8 | **−12.2** |
| vwap | 100.0% | −76.7 | −23.2 |

**Why.** VWAP's forecast is the average of META's *other* sessions —
which are U-shaped — so it front- and back-loads a day that never
develops those humps. TWAP assumes nothing about shape and is closer to
right precisely because today has no shape.

**The metric: `vs vwap` again, distance from zero.** TWAP tracks at
12.2bps, VWAP at 23.2. Read this pair with case 1: VWAP is not better
than TWAP, it is a bet that today resembles history.

**Fairness note.** VWAP's forecast is built leave-one-out — the session
being traded is excluded — so it can genuinely be wrong. Letting it peek
at today's own curve would make this case impossible.

---

## 3. POV keeps the smallest footprint, and waits for its volume

**Shows:** POV's real promise is market share, not price or punctuality —
and both edges of that bargain.

```csv
timestamp,symbol,side,quantity,deadline
11:30:00,META,BUY,180000,13:30:00
```

**Market condition — META 2026-07-17, 11:30–13:30.** One minute (12:18)
trades 406,286 shares, sixteen times the window's median minute. No
forecast contains it, and the tape rises through the window.

**Compare:** `pov` at 3% · `twap` · `vwap`.

| config | filled | shortfall | impact c/sh | timing $ |
| --- | --- | --- | --- | --- |
| pov 3% | 100.0% | 218.9 | **4.95** | 2,470,102 |
| twap | 100.0% | 175.3 | 5.59 | 1,974,374 |
| vwap | 100.0% | 162.9 | 5.66 | 1,833,308 |

**Why.** POV does its heaviest trading exactly where the volume is, so
each of its shares moves the price least — the lowest impact of the
three. But it makes no promise about *when*, and here the liquidity
arrives late in a rising market, so it buys more of its order at higher
prices. Cheapest footprint, worst drift.

**The metrics: `impact c/sh` and `timing $`, side by side.** Shortfall
alone would say POV lost; the decomposition shows *what* it lost and what
it bought with that loss. This is the case that justifies the cost tree.

---

## 4. Implementation shortfall outruns adverse drift

**Shows:** when the market moves against you after the signal, urgency is
worth paying for — and the effect is monotone in the knob.

```csv
timestamp,symbol,side,quantity,deadline
10:00:00,NFLX,BUY,600000,11:00:00
```

**Market condition — NFLX 2026-07-17, 10:00–11:00.** The tape runs
**+256bps against the buyer** inside the hour, the worst adverse drift in
the catalog for that window.

**Compare:** `is` at urgency 4 · `is` at urgency 2 · `twap` · `immediate`.

| config | filled | shortfall | timing $ |
| --- | --- | --- | --- |
| is urgency 4 | 100.0% | **111.5** | 404,907 |
| is urgency 2 | 100.0% | 125.7 | 466,317 |
| twap | 100.0% | 136.7 | 510,700 |
| immediate | 6.6% | 14.9 | 0 |

**Why.** The Almgren–Chriss schedule front-loads: the more urgent the
setting, the more of the order is done while the price is still near
arrival. Timing cost falls monotonically with urgency — 511k → 466k →
405k — and shortfall follows it down.

**The metric: `timing $`.** It isolates the market's own move from the
damage we did ourselves, which is the whole claim. Note that urgency 4
pays *more* impact per share than urgency 2 (5.17¢ vs 4.44¢): it is
buying protection from drift with impact, which is exactly the trade the
algorithm exists to make.

**Note on immediate:** at 6.6% filled it is not a comparable row — 600,000
shares cannot clear one minute. Case 5 is the version where immediacy is
actually feasible.

---

## 5. When the signal decays in minutes, only immediacy captures it

**Shows:** case 4 taken to its limit — when the whole move happens in the
first few minutes, even a front-loaded schedule is too slow.

```csv
timestamp,symbol,side,quantity,deadline
10:00:00,META,BUY,20000,10:15:00
```

**Market condition — META 2026-07-09, 10:00.** 125 of the hour's 153bps
of adverse move land in the **first five minutes**. The order is 9% of
the arrival minute's 220,183 shares — inside the participation cap, so
immediacy can genuinely finish.

**Compare:** `immediate` · `is` at urgency 4 · `twap`.

| config | filled | shortfall | vs immed $ |
| --- | --- | --- | --- |
| immediate | 100.0% | **1.7** | 0 |
| is urgency 4 | 100.0% | 98.0 | −113,727 |
| twap | 100.0% | 66.4 | −76,440 |

**Why.** There is no drift to spread into: the edge is gone before the
second slice. Immediacy pays the spread and 8.0¢ of impact — the model's
ceiling — and still wins by ~65–96bps, because the alternative is buying
into a price that has already left.

**The metric: `shortfall`, with `vs immed $` confirming it in dollars.**
Both schedules destroy value against the baseline.

**Sizing is the assumption that makes this fair.** Multiply this order by
five and immediacy cannot complete, and the answer inverts — which is
precisely case 6.

---

## 6. The same order, three deadlines: what haste actually costs

**Shows:** aggression as a *parameter*, isolated. One algorithm, one
order, one session; only the deadline changes.

```csv
timestamp,symbol,side,quantity,deadline
10:00:00,GOOG,BUY,120000,15:59:00     # demos/06_impact_of_haste.csv
10:00:00,GOOG,BUY,120000,12:00:00     # ..._hour.csv
10:00:00,GOOG,BUY,120000,10:30:00     # ..._rushed.csv
```

**Market condition — GOOG 2026-07-13.** The quietest session in the
catalog (5.85M shares all day). The 10:00–10:30 window carries 487,276
shares, so the order is a quarter of it; the rest of the day carries
4.6M, where the same order is a rounding error.

**Compare:** `twap` against each deadline.

| config | filled | impact c/sh |
| --- | --- | --- |
| due 15:59 | **100.0%** | **4.75** |
| due 12:00 | 98.3% | 7.17 |
| due 10:30 | 39.7% | 8.00 |

**Why.** A tighter deadline concentrates the same shares into fewer
minutes, so each one is a larger fraction of its bar — and impact grows
with participation. Past a point the participation cap binds and the
order simply cannot finish: at 10:30 it gets 39.7% done while paying the
most per share for the privilege.

**The metrics: `impact c/sh` and `filled`, which must be read together.**
Total impact dollars would *fall* for the rushed run purely because it
filled less — a trap worth knowing about. Per share, haste is strictly
more expensive: 4.75¢ → 7.17¢ → 8.00¢.

**Honest limitation.** In this simulator extreme aggression shows up as
incompletion (the participation cap) rather than an unbounded price
spiral. Engine B, where impact emerges from walking a real book, is the
version that pushes the price instead of refusing the trade.

---

## 7. One knob, both horns: participation rate

**Shows:** a pure parameter trade-off, with the two metrics moving in
opposite directions and nothing else changed.

```csv
timestamp,symbol,side,quantity,deadline
10:00:00,GOOG,BUY,150000,12:00:00
```

**Market condition — GOOG 2026-07-13, 10:00–12:00.** The order is 10.7%
of the window's 1.64M shares: large enough that the rate decides whether
it finishes.

**Compare:** `pov` at 2% · 6% · 15%. Same algorithm, same order.

| config | filled | impact c/sh | vs immed $ |
| --- | --- | --- | --- |
| pov 2% | 22.5% | **4.34** | −60,971 |
| pov 6% | 65.7% | 6.74 | −199,879 |
| pov 15% | **100.0%** | 8.00 | −312,611 |

**Why.** POV's rate is a direct dial between footprint and certainty.
Low rates buy cheaply and may never finish; high rates finish and pay
for it. There is no setting that wins both columns.

**The metrics: `filled` against `impact c/sh`.** They move in opposite
directions monotonically — 22.5% → 65.7% → 100% while 4.34¢ → 6.74¢ →
8.00¢. Which end is right depends on whether the unfilled remainder was
alpha you needed, which is what `opportunity cost` measures in the full
report.

**Reading the negative value-add:** GOOG fell that day, so *any* buying
lost money against a baseline that barely traded. That is direction, not
execution quality — a reminder that value-add compares executions only
when both are executing the same alpha in the same market.

---

## Assumptions that make these fair and reproducible

1. **Fixed market data.** Every session is a committed CSV under `data/`.
   No live feeds, no regeneration.
2. **Fixed seeds.** Engine B's randomness comes from a seeded 32-bit LCG,
   and these demos run Engine A (the deterministic bar model) unless
   stated. Same input, same fills, on any machine and in the browser.
3. **No look-ahead.** Algorithms see only the *previous* bar when
   deciding. VWAP's forecast profile is the leave-one-out average of the
   symbol's other sessions — never the day being traded.
4. **One variable per comparison.** Within a demo, configurations differ
   in exactly one thing. The fill model's knobs (2¢ half-spread, 10%
   participation cap, 25¢ impact coefficient) are identical everywhere.
5. **The same benchmark for everyone.** Arrival price is sampled at the
   first minute in which the algorithm could actually trade, so no
   configuration is charged for a price it could never have reached.
6. **Immediate is the baseline, not a strawman.** It is a real algorithm
   here, and it wins cases 5 outright — and would win more if every order
   were small.
7. **Absolute costs are model artifacts; comparisons are the product.**
   The invented spread, the participation cap and the stateless impact
   formula distort every configuration identically, so rankings and
   value-add are meaningful even though "your true P&L" is not.
