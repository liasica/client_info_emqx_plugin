# client_info_emqx_plugin

> MQTT 消息客户端信息注入插件

一个 EMQX 插件，当消息发布到指定 Topic 时，自动将客户端的 IP 地址和 ClientId 以二进制头部的形式注入到 Payload 开头，便于下游服务无需额外查询即可识别消息来源。

## 功能特性

- 根据配置的 Topic 列表（支持 MQTT 通配符 `#` 和 `+`）过滤消息
- 每个 Topic 可独立启用/禁用，无需修改配置即可快速切换
- 在原始 Payload 前附加固定 Magic 头部 + 客户端 IP + ClientId
- 支持 IPv4（4 字节）和 IPv6（16 字节）

## 附加数据结构

采用**字段数量 + 逐字段长度前缀**的可扩展格式，未来增加新字段无需修改现有解析逻辑。

```
+--------------------+-------------+-------------------------------+-------------------------------+------------------+
| Magic (4B)         | Count (1B)  | FieldLen[0] (2B, BE)          | FieldLen[1] (2B, BE)          |                  |
| 01 35 83 08        | 字段总数    | Field[0] 数据（FieldLen[0]B） | Field[1] 数据（FieldLen[1]B） | original payload |
+--------------------+-------------+-------------------------------+-------------------------------+------------------+
```

| 段          | 大小             | 说明                           |
| ----------- | ---------------- | ------------------------------ |
| Magic       | 4 字节           | 固定标识 `0x01 0x35 0x83 0x08` |
| FieldCount  | 1 字节           | 字段总数（当前为 `2`）         |
| FieldLen[i] | 2 字节大端       | 第 i 个字段的字节长度          |
| Field[i]    | FieldLen[i] 字节 | 第 i 个字段的数据              |
| Payload     | 剩余             | 原始 MQTT 消息内容             |

**当前字段定义（按顺序）：**

| 索引 | 字段     | 编码规则                    |
| ---- | -------- | --------------------------- |
| 0    | peerhost | IPv4：4 字节；IPv6：16 字节 |
| 1    | clientid | UTF-8 编码的客户端 ID       |

### 示例

IPv4 `192.168.1.100`（4 字节）+ ClientId `dev-01`（6 字节）：

```
01 35 83 08        -- Magic
02                 -- FieldCount = 2
00 04              -- Field[0] len = 4 (peerhost, IPv4)
C0 A8 01 64        -- 192.168.1.100
00 06              -- Field[1] len = 6 (clientid)
64 65 76 2D 30 31  -- "dev-01"
<原始 payload>
```

### 可扩展性

解析方只需按以下步骤处理：

1. 读取并校验 4 字节 Magic
2. 读取 1 字节 `FieldCount`
3. 循环 `FieldCount` 次：读取 2 字节长度 → 读取对应字节数的数据
4. 剩余所有字节为原始 Payload

将来新增字段只需在列表末尾追加，旧版解析方可安全忽略未知字段。

## 使用方法

1. 执行 `make rel` 构建插件包，或从 [Releases](../../releases) 下载最新版本
2. 在 EMQX Dashboard 中上传并安装插件
3. 启用插件后进入 **管理插件** 配置界面
4. 在 **管理的 Topic 列表** 表格中添加需要处理的 Topic，并设置启用状态：

   | 字段    | 说明                                   |
   | ------- | -------------------------------------- |
   | Topic   | MQTT Topic，支持通配符，如 `client/#`  |
   | Enabled | 开关，控制该条目是否参与消息头附加处理 |

5. 点击 **保存修改**，配置立即生效（无需重启插件）

## 配置说明

配置存储格式为 JSON 数组，每个元素包含 `topic` 和 `enabled` 两个字段：

- `enabled` 默认启用（缺失或为 `null` 时视为启用）
- 仅当 `enabled` 显式为 `false` 时，该条目被禁用
- 在部分 Dashboard/Avro union 场景下，`enabled` 可能被序列化为 `{"boolean": true|false}`，与布尔值语义等价

```json
[
  { "topic": "client/#", "enabled": true },
  { "topic": "device/+/status", "enabled": false }
]
```

## 故障排查

- 现象：界面显示“未启用”，但配置中 `enabled` 看起来是启用值
- 原因：Dashboard 在 Avro union 场景下可能使用 `{"boolean": true}` 这类包装格式
- 说明：`true` 与 `{"boolean": true}` 语义等价，插件会按“启用”处理
- 建议：优先通过界面开关修改并保存；如需手工编辑配置，建议使用 union 包装格式保持显示一致

## 开发

### 环境要求

- Erlang/OTP 28+
- rebar3（`make ensure-rebar3` 自动下载）

### 常用命令

```shell
# 下载/更新依赖
rebar3 upgrade --all

# 编译
make compile

# 构建插件包
make rel

# 格式化代码
make fmt

# 运行单元测试
make eunit

# 运行 CT 测试
make ct
```

### 构建产物

```
_build/default/emqx_plugrel/client_info_emqx_plugin-<vsn>.tar.gz
_build/default/emqx_plugrel/client_info_emqx_plugin-<vsn>.sha256
```

## IDE 配置

![Xnip2025-11-13_14-13-13.png](docs/Xnip2025-11-13_14-13-13.png)
![Xnip2025-11-13_14-14-04.png](docs/Xnip2025-11-13_14-14-04.png)

## 参考文档

- [EMQX 插件扩展文档](https://docs.emqx.com/zh/emqx/latest/extensions/plugins.html)
- [EMQX .tool-versions](https://github.com/emqx/emqx/blob/e6.0.1/.tool-versions)
- [emqx-plugin-helper](https://github.com/emqx/emqx-plugin-helper)
