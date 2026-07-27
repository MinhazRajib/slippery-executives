# Historical market data

Bundled 1-minute OHLCV bars, one file per symbol per trading day:

```
data/<SYMBOL>/<YYYY-MM-DD>.csv      e.g. data/NVDA/2024-03-14.csv
```

## File format

Header row required, then one row per minute, in ascending time order:

```
time,open,high,low,close,volume
09:30:00,150.20,150.45,150.10,150.38,42000
09:31:00,150.38,150.52,150.30,150.44,31500
```

- `time` — bar start, exchange-local (US/Eastern) time of day, `HH:MM:SS`.
  Regular session only: `09:30:00` through `15:59:00` (390 bars).
- `open,high,low,close` — decimal dollars. Sub-cent precision is allowed
  in the raw file; the loader rounds to the nearest cent on load (a
  documented approximation of the platform).
- `volume` — shares traded during the minute, non-negative integer.

Use raw (unadjusted) prices, not split/dividend-adjusted ones — the
simulation replays the actual session.

The data loader (`lib/market`) validates on load: ascending unique
timestamps, valid OHLC relationships (`low <= open,close <= high`),
non-negative volume, and market-hours coverage.
