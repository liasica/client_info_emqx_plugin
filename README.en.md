# client_info_emqx_plugin

> MQTT Message Client Info Injection Plugin

An EMQX plugin that automatically injects client information (IP address and ClientId) as a binary header at the beginning of the message payload when a message is published to a configured topic. This allows downstream services to identify the message origin without any additional queries.

## Features

- Filters messages by a configurable topic list with MQTT wildcard support (`#` and `+`)
- Each topic entry can be individually enabled or disabled without restarting the plugin
- Prepends a fixed Magic header + client IP + ClientId to the original payload
- Supports both IPv4 (4 bytes) and IPv6 (16 bytes)

## Payload Structure

An extensible **field-count + per-field length-prefix** format is used. Adding new fields in the future requires no changes to existing parsers.

```
+--------------------+-------------+-------------------------------+-------------------------------+------------------+
| Magic (4B)         | Count (1B)  | FieldLen[0] (2B, BE)          | FieldLen[1] (2B, BE)          |                  |
| 01 35 83 08        | # of fields | Field[0] data (FieldLen[0]B)  | Field[1] data (FieldLen[1]B)  | original payload |
+--------------------+-------------+-------------------------------+-------------------------------+------------------+
```

| Segment     | Size          | Description                            |
| ----------- | ------------- | -------------------------------------- |
| Magic       | 4 bytes       | Fixed identifier `0x01 0x35 0x83 0x08` |
| FieldCount  | 1 byte        | Total number of fields (currently `2`) |
| FieldLen[i] | 2 bytes (BE)  | Byte length of the i-th field          |
| Field[i]    | FieldLen[i] B | Data of the i-th field                 |
| Payload     | remainder     | Original MQTT message payload          |

**Current fields (in order):**

| Index | Field    | Encoding                      |
| ----- | -------- | ----------------------------- |
| 0     | peerhost | IPv4: 4 bytes; IPv6: 16 bytes |
| 1     | clientid | Client ID as UTF-8 bytes      |

### Example

IPv4 `192.168.1.100` (4 bytes) + ClientId `dev-01` (6 bytes):

```
01 35 83 08        -- Magic
02                 -- FieldCount = 2
00 04              -- Field[0] len = 4 (peerhost, IPv4)
C0 A8 01 64        -- 192.168.1.100
00 06              -- Field[1] len = 6 (clientid)
64 65 76 2D 30 31  -- "dev-01"
<original payload>
```

### Extensibility

Parsers only need to follow these steps:

1. Read and verify the 4-byte Magic
2. Read 1 byte `FieldCount`
3. Loop `FieldCount` times: read 2-byte length → read that many bytes of data
4. All remaining bytes are the original Payload

New fields can be appended to the list at any time; older parsers can safely skip unknown fields.

## Usage

1. Build the plugin package with `make rel`, or download the latest release from [Releases](../../releases)
2. Upload and install the package via the EMQX Dashboard
3. Enable the plugin and open the **Manage Plugin** configuration page
4. Add topics to the **Managed Topics** table and configure their enabled state:

   | Field   | Description                                      |
   | ------- | ------------------------------------------------ |
   | Topic   | MQTT topic, wildcards supported, e.g. `client/#` |
   | Enabled | Toggle to control whether this entry is active   |

5. Click **Save**, changes take effect immediately without restarting the plugin

## Configuration

The configuration is stored as a JSON array. Each element contains a `topic` and an `enabled` field:

- `enabled` is on by default (missing or `null` is treated as enabled)
- A topic entry is disabled only when `enabled` is explicitly `false`
- In some Dashboard/Avro union cases, `enabled` may be serialized as `{"boolean": true|false}`, which is semantically equivalent to a plain boolean

```json
[
  { "topic": "client/#", "enabled": true },
  { "topic": "device/+/status", "enabled": false }
]
```

## Troubleshooting

- Symptom: the UI shows "Disabled" while `enabled` appears to be an enabled value in raw config
- Cause: in Avro union scenarios, Dashboard may use wrapped values like `{"boolean": true}`
- Note: `true` and `{"boolean": true}` are semantically equivalent, and the plugin treats both as enabled
- Recommendation: prefer toggling and saving through the UI; if editing raw config manually, use the union-wrapped form for consistent UI display

## Development

### Requirements

- Erlang/OTP 28+
- rebar3 (auto-downloaded via `make ensure-rebar3`)

### Common Commands

```shell
# Update dependencies
rebar3 upgrade --all

# Compile
make compile

# Build plugin package
make rel

# Format code
make fmt

# Run unit tests
make eunit

# Run common tests
make ct
```

### Build Output

```
_build/default/emqx_plugrel/client_info_emqx_plugin-<vsn>.tar.gz
_build/default/emqx_plugrel/client_info_emqx_plugin-<vsn>.sha256
```

## IDE Setup

![Xnip2025-11-13_14-13-13.png](docs/Xnip2025-11-13_14-13-13.png)
![Xnip2025-11-13_14-14-04.png](docs/Xnip2025-11-13_14-14-04.png)

## References

- [EMQX Plugin Documentation](https://docs.emqx.com/en/emqx/latest/extensions/plugins.html)
- [EMQX .tool-versions](https://github.com/emqx/emqx/blob/e6.0.1/.tool-versions)
- [emqx-plugin-helper](https://github.com/emqx/emqx-plugin-helper)
