# client_info_emqx_plugin

> MQTT 消息客户端信息注入插件

一个 EMQX 插件，当消息发布到指定 Topic 时，自动将客户端的 IP 地址和 ClientId 以二进制头部的形式注入到 Payload 开头，便于下游服务无需额外查询即可识别消息来源。

## 功能特性

- 根据配置的 Topic 列表（支持 MQTT 通配符 `#` 和 `+`）过滤消息
- 每个 Topic 可独立启用/禁用，无需修改配置即可快速切换
- 在原始 Payload 前附加固定 Magic 头部 + 客户端 IP + ClientId
- 支持 IPv4（4 字节）和 IPv6（16 字节）

## 附加数据结构

```
| 0x01 0x35 0x83 0x08 | 0xNN 0xNN 0xNN 0xNN |  peerhost   |   clientid   | original payload |
|      Magic (4B)     | len = N+M (4B, BE)  |  4 or 16B   |    M bytes   |                  |
```

- **Magic**：固定标识字节 `0x01 0x35 0x83 0x08`
- **len**：4 字节大端无符号整数，值 = `byte_size(peerhost) + byte_size(clientid)`，不含 Magic 和 len 自身
- **peerhost**：IPv4 为 4 字节，IPv6 为 16 字节
- **clientid**：UTF-8 编码的客户端 ID 字节序列

### 示例

IPv4 `192.168.1.100`（4 字节）+ ClientId `dev-01`（6 字节）：

```
01 35 83 08  00 00 00 0A  C0 A8 01 64  64 65 76 2D 30 31  <原始 payload>
             ↑ len=10(4+6) ↑ 192.168.1.100               ↑ "dev-01"
```

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
