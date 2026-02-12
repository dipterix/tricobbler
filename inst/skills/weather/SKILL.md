---
name: weather
description: Get current weather and forecasts (no API key required)
metadata:
  openclaw:
    emoji: "🌤️"
    requires: 
      bins: ["curl"]
---

# Weather

Provides weather information using free services that require no API keys.

## fetch_weather

Fetch current weather conditions and forecasts for a given location.

Uses [wttr.in](https://wttr.in) as the primary service:

```bash
# Current conditions (concise)
curl -s "wttr.in/London?format=3"

# Detailed forecast
curl -s "wttr.in/London"

# JSON format for parsing
curl -s "wttr.in/London?format=j1"

# Specific format strings
curl -s "wttr.in/London?format=%l:+%C+%t+%w+%h+%p"
```

### Parameters

- **location**: City name, airport code (e.g., `JFK`), domain (`@domain.com`),
  or GPS coordinates (`48.8566,2.3522`).
- **format**: Output format — plain text (default), `j1` for JSON, `j2` for
  extended JSON, or custom format strings using `%`-codes.
- **lang**: Language code (e.g., `fr`, `de`, `zh`).

### Fallback: Open-Meteo API

If wttr.in is unreachable, use [Open-Meteo](https://open-meteo.com/) (free,
no key required):
```bash
curl -s "https://api.open-meteo.com/v1/forecast?latitude=51.5&longitude=-0.1&current_weather=true"
```

## format_weather

Format raw weather data into a human-readable summary.

Given a JSON response from wttr.in or Open-Meteo, produce a concise summary
including temperature, conditions, wind speed, and humidity.
