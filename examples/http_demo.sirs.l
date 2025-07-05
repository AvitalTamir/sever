{
  "program": {
    "entry": "main",
    "functions": {
      "main": {
        "args": [],
        "return": "i32",
        "body": [
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "HTTP Server Feature Demo"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "========================"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": ""}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "New in Sever: HTTP Server Capabilities!"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": ""}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "You can now create HTTP servers with:"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "  http_serve(port, handler_function)"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": ""}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "Handler functions receive:"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "  - method: str (GET, POST, etc.)"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "  - path: str (requested URL path)"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "  - body: str (request body)"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": ""}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "Perfect for building:"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "  - REST APIs"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "  - Anomaly detection endpoints"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "  - Microservices"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "  - Web services"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": ""}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "Combined with Sever's probabilistic programming,"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "you can build powerful ML-powered APIs!"}]
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