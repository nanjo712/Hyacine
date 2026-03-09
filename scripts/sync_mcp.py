import json
import os

# 定义文件路径
METALS_CONFIG = ".metals/mcp.json"
# OpenCode 配置文件路径 (通常在 .opencode/opencode.json 或根目录下)
OPENCODE_CONFIG = "opencode.json" 

def sync():
    # 1. 读取 Metals 生成的配置
    if not os.path.exists(METALS_CONFIG):
        print(f"❌ 未找到 {METALS_CONFIG}，请确保 Metals 已启动。")
        return

    with open(METALS_CONFIG, 'r') as f:
        metals_data = json.load(f)

    # 2. 读取或初始化 OpenCode 配置
    if os.path.exists(OPENCODE_CONFIG):
        with open(OPENCODE_CONFIG, 'r') as f:
            try:
                op_data = json.load(f)
            except json.JSONDecodeError:
                op_data = {}
    else:
        op_data = {}

    # 确保 mcp 字段存在
    if "mcp" not in op_data:
        op_data["mcp"] = {}

    # 3. 转换格式并合并
    # Metals 格式: {"servers": {"Name": {"url": "..."}}}
    # OpenCode 格式: {"mcp": {"Name": {"type": "remote", "url": "...", "enabled": true}}}
    
    servers = metals_data.get("servers", {})
    updated_count = 0
    
    for name, info in servers.items():
        op_data["mcp"][name] = {
            "type": "remote",
            "url": info["url"],
            "enabled": True
        }
        updated_count += 1
        print(f"✅ 同步服务: {name} -> {info['url']}")

    # 4. 写回文件
    with open(OPENCODE_CONFIG, 'w') as f:
        json.dump(op_data, f, indent=2)
    
    print(f"🎉 成功更新 {OPENCODE_CONFIG}，共 {updated_count} 个服务。")

if __name__ == "__main__":
    sync()