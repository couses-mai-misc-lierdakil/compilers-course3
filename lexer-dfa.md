```
<токен> ::= <шаблон>
number ::= /[0-9]+(\.[0-9]*)?([eE][+-]?[0-9]+)?/
operator ::= /[+*/^%-]/
id ::= /[a-z_][a-z_0-9]*/
lparen ::= /\(/
rparen ::= /\)/
comma ::= /,/
```

```dot
digraph {
  rankdir=LR;
  node[shape=circle];
  1  [peripheries=2; xlabel="number"  ];
  2  [peripheries=2; xlabel="number"  ];
  5  [peripheries=2; xlabel="number"  ];
  6  [peripheries=2; xlabel="operator"];
  7  [peripheries=2; xlabel="id"      ];
  8  [peripheries=2; xlabel="lparen"  ];
  9  [peripheries=2; xlabel="rparen"  ];
  10 [peripheries=2; xlabel="comma"   ];
  0 -> 7  [label="[a-z_]"   ];
  0 -> 8  [label="'('"        ];
  0 -> 9  [label="')'"        ];
  0 -> 10 [label="','"        ];
  0 -> 6  [label="[+*/^%-]" ];
  0 -> 1  [label="[0-9]"    ];
  1 -> 1  [label="[0-9]"    ];
  1 -> 2  [label="'.'"       ];
  1 -> 3  [label="[eE]"     ];
  2 -> 2  [label="[0-9]"    ];
  2 -> 3  [label="[eE]"     ];
  3 -> 4  [label="[+-]"     ];
  3 -> 5  [label="[0-9]"    ];
  4 -> 5  [label="[0-9]"    ];
  5 -> 5  [label="[0-9]"    ];
  7 -> 7  [label="[a-z_0-9]"];
}
```


| состояние | символ   | новое состояние |
|-----------|----------|-----------------|
| 0         | [a-z_]   | 7               |
| 0         | (        | 8               |
| 0         | )        | 9               |
| 0         | ,        | 10              |
| 0         | [+*/^%-] | 6               |
| 0         | [0-9]    | 1               |
| 1         | [0-9]    | 1               |
| 1         | \.       | 2               |
| 1         | [eE]     | 3               |
| 2         | [0-9]    | 2               |
| 2         | [eE]     | 3               |
| 3         | [+-]     | 4               |
| 3         | [0-9]    | 5               |
| 4         | [0-9]    | 5               |
| 5         | [0-9]    | 5               |
| 7         | [a-z_0-9]| 7               |

| состояние | токен    |
|-----------|----------|
| 1         | number   |
| 2         | number   |
| 5         | number   |
| 6         | operator |
| 7         | id       |
| 8         | lparen   |
| 9         | rparen   |
| 10        | comma    |
