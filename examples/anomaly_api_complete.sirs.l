{
  "program": {
    "entry": "main",
    "functions": {
      "calculate_anomaly_score": {
        "args": [
          {"name": "error_rate", "type": "f64"},
          {"name": "response_time", "type": "f64"}
        ],
        "return": "str",
        "body": [
          {
            "let": {
              "name": "baseline_error_rate",
              "type": "f64",
              "value": {"literal": 0.05}
            }
          },
          {
            "let": {
              "name": "baseline_response_time",
              "type": "f64",
              "value": {"literal": 100.0}
            }
          },
          {
            "let": {
              "name": "error_score",
              "type": "f64",
              "value": {
                "op": {
                  "kind": "div",
                  "args": [
                    {"var": "error_rate"},
                    {"var": "baseline_error_rate"}
                  ]
                }
              }
            }
          },
          {
            "let": {
              "name": "response_score",
              "type": "f64",
              "value": {
                "op": {
                  "kind": "div",
                  "args": [
                    {"var": "response_time"},
                    {"var": "baseline_response_time"}
                  ]
                }
              }
            }
          },
          {
            "let": {
              "name": "total_score",
              "type": "f64",
              "value": {
                "op": {
                  "kind": "div",
                  "args": [
                    {
                      "op": {
                        "kind": "add",
                        "args": [
                          {"var": "error_score"},
                          {"var": "response_score"}
                        ]
                      }
                    },
                    {"literal": 2.0}
                  ]
                }
              }
            }
          },
          {
            "let": {
              "name": "status",
              "type": "str",
              "mutable": true,
              "value": {"literal": "normal"}
            }
          },
          {
            "if": {
              "condition": {
                "op": {
                  "kind": "gt",
                  "args": [{"var": "total_score"}, {"literal": 2.0}]
                }
              },
              "then": [
                {
                  "assign": {
                    "target": {"var": "status"},
                    "value": {"literal": "critical"}
                  }
                }
              ],
              "else": [
                {
                  "if": {
                    "condition": {
                      "op": {
                        "kind": "gt",
                        "args": [{"var": "total_score"}, {"literal": 1.5}]
                      }
                    },
                    "then": [
                      {
                        "assign": {
                          "target": {"var": "status"},
                          "value": {"literal": "warning"}
                        }
                      }
                    ]
                  }
                }
              ]
            }
          },
          {
            "let": {
              "name": "response_json",
              "type": "str",
              "mutable": true,
              "value": {
                "call": {
                  "function": "json_create_object",
                  "args": []
                }
              }
            }
          },
          {
            "assign": {
              "target": {"var": "response_json"},
              "value": {
                "call": {
                  "function": "json_add_string",
                  "args": [
                    {"var": "response_json"},
                    {"literal": "status"},
                    {"var": "status"}
                  ]
                }
              }
            }
          },
          {
            "assign": {
              "target": {"var": "response_json"},
              "value": {
                "call": {
                  "function": "json_add_number",
                  "args": [
                    {"var": "response_json"},
                    {"literal": "anomaly_score"},
                    {"var": "total_score"}
                  ]
                }
              }
            }
          },
          {
            "assign": {
              "target": {"var": "response_json"},
              "value": {
                "call": {
                  "function": "json_add_number",
                  "args": [
                    {"var": "response_json"},
                    {"literal": "error_rate"},
                    {"var": "error_rate"}
                  ]
                }
              }
            }
          },
          {
            "assign": {
              "target": {"var": "response_json"},
              "value": {
                "call": {
                  "function": "json_add_number",
                  "args": [
                    {"var": "response_json"},
                    {"literal": "response_time"},
                    {"var": "response_time"}
                  ]
                }
              }
            }
          },
          {
            "return": {
              "var": "response_json"
            }
          }
        ]
      },
      "main": {
        "args": [],
        "return": "i32",
        "body": [
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "Sever Anomaly Detection API"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "============================="}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "Testing anomaly detection with JSON responses..."}]
              }
            }
          },
          {
            "let": {
              "name": "result1",
              "type": "str",
              "value": {
                "call": {
                  "function": "calculate_anomaly_score",
                  "args": [
                    {"literal": 0.02},
                    {"literal": 95.0}
                  ]
                }
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "Test 1 (Normal): "}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"var": "result1"}]
              }
            }
          },
          {
            "let": {
              "name": "result2",
              "type": "str",
              "value": {
                "call": {
                  "function": "calculate_anomaly_score",
                  "args": [
                    {"literal": 0.12},
                    {"literal": 180.0}
                  ]
                }
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "Test 2 (Warning): "}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"var": "result2"}]
              }
            }
          },
          {
            "let": {
              "name": "result3",
              "type": "str",
              "value": {
                "call": {
                  "function": "calculate_anomaly_score",
                  "args": [
                    {"literal": 0.25},
                    {"literal": 300.0}
                  ]
                }
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "Test 3 (Critical): "}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"var": "result3"}]
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
                "args": [{"literal": "Ready for HTTP server integration!"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "http_serve",
                "args": [
                  {"literal": 8080},
                  {"literal": "calculate_anomaly_score"}
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