{
  "program": {
    "entry": "main",
    "functions": {
      "main": {
        "args": [],
        "return": "i32",
        "body": [
          {
            "let": {
              "name": "port",
              "type": "i32",
              "value": {
                "literal": 8080
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "Starting Sever HTTP Server..."}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "http_serve",
                "args": [
                  {"var": "port"},
                  {"literal": "anomaly_handler"}
                ]
              }
            }
          },
          {
            "return": {
              "literal": 0
            }
          }
        ]
      }
    }
  }
}