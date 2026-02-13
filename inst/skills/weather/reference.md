# Weather API Reference

## Endpoints

- `wttr.in/:location` - Get weather for a location
- `wttr.in/:location?format=j1` - Get weather in JSON format

## Parameters

| Parameter | Description |
|-----------|-------------|
| location  | City name, airport code, or coordinates |
| format    | Output format: j1 (JSON), j2 (JSON v2) |

## Example

```
curl "wttr.in/Boston?format=j1"
```
