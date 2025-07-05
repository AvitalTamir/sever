{
  "program": {
    "entry": "main",
    "functions": {
      "handle_anomaly_check": {
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
              "name": "error_anomaly",
              "type": "f64",
              "mutable": true,
              "value": {"literal": 0.0}
            }
          },
          {
            "if": {
              "condition": {
                "op": {
                  "kind": "gt",
                  "args": [
                    {"var": "error_rate"},
                    {
                      "op": {
                        "kind": "mul",
                        "args": [
                          {"var": "baseline_error_rate"},
                          {"literal": 2.0}
                        ]
                      }
                    }
                  ]
                }
              },
              "then": [
                {
                  "assign": {
                    "target": {"var": "error_anomaly"},
                    "value": {
                      "op": {
                        "kind": "div",
                        "args": [
                          {
                            "op": {
                              "kind": "sub",
                              "args": [
                                {"var": "error_rate"},
                                {"var": "baseline_error_rate"}
                              ]
                            }
                          },
                          {"var": "baseline_error_rate"}
                        ]
                      }
                    }
                  }
                }
              ]
            }
          },
          {
            "let": {
              "name": "response_anomaly",
              "type": "f64",
              "mutable": true,
              "value": {"literal": 0.0}
            }
          },
          {
            "if": {
              "condition": {
                "op": {
                  "kind": "gt",
                  "args": [
                    {"var": "response_time"},
                    {
                      "op": {
                        "kind": "mul",
                        "args": [
                          {"var": "baseline_response_time"},
                          {"literal": 1.5}
                        ]
                      }
                    }
                  ]
                }
              },
              "then": [
                {
                  "assign": {
                    "target": {"var": "response_anomaly"},
                    "value": {
                      "op": {
                        "kind": "div",
                        "args": [
                          {
                            "op": {
                              "kind": "sub",
                              "args": [
                                {"var": "response_time"},
                                {"var": "baseline_response_time"}
                              ]
                            }
                          },
                          {"var": "baseline_response_time"}
                        ]
                      }
                    }
                  }
                }
              ]
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
                          {"var": "error_anomaly"},
                          {"var": "response_anomaly"}
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
            "let": {
              "name": "severity",
              "type": "str",
              "mutable": true,
              "value": {"literal": "none"}
            }
          },
          {
            "if": {
              "condition": {
                "op": {
                  "kind": "gt",
                  "args": [{"var": "total_score"}, {"literal": 0.8}]
                }
              },
              "then": [
                {
                  "assign": {
                    "target": {"var": "status"},
                    "value": {"literal": "critical"}
                  }
                },
                {
                  "assign": {
                    "target": {"var": "severity"},
                    "value": {"literal": "high"}
                  }
                }
              ],
              "else": [
                {
                  "if": {
                    "condition": {
                      "op": {
                        "kind": "gt",
                        "args": [{"var": "total_score"}, {"literal": 0.5}]
                      }
                    },
                    "then": [
                      {
                        "assign": {
                          "name": "status",
                          "value": {"literal": "warning"}
                        }
                      },
                      {
                        "assign": {
                          "name": "severity",
                          "value": {"literal": "medium"}
                        }
                      }
                    ],
                    "else": [
                      {
                        "if": {
                          "condition": {
                            "op": {
                              "kind": "gt",
                              "args": [{"var": "total_score"}, {"literal": 0.2}]
                            }
                          },
                          "then": [
                            {
                              "assign": {
                                "name": "status",
                                "value": {"literal": "anomaly"}
                              }
                            },
                            {
                              "assign": {
                                "name": "severity",
                                "value": {"literal": "low"}
                              }
                            }
                          ]
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
              "name": "response",
              "type": "str",
              "value": {"literal": "{"}
            }
          },
          {
            "return": {
              "var": "response"
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
                "args": [{"literal": "Anomaly Detection API Demo"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "=========================="}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "Test 1: Normal metrics"}]
              }
            }
          },
          {
            "let": {
              "name": "result1",
              "type": "str",
              "value": {
                "call": {
                  "function": "handle_anomaly_check",
                  "args": [
                    {"literal": 0.03},
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
                "args": [{"var": "result1"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "Test 2: High error rate"}]
              }
            }
          },
          {
            "let": {
              "name": "result2",
              "type": "str",
              "value": {
                "call": {
                  "function": "handle_anomaly_check",
                  "args": [
                    {"literal": 0.15},
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
                "args": [{"var": "result2"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "HTTP server capabilities are now available!"}]
              }
            }
          },
          {
            "expression": {
              "call": {
                "function": "std_print",
                "args": [{"literal": "Use http_serve(port, handler) to create REST APIs"}]
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