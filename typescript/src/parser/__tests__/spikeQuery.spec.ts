import {parseStructuralQuery} from "../spikeQuery";

describe("parseStructuralQuery", () => {
  test("only tokens", () => {
    const query = "abc and def"
    expect(parseStructuralQuery(query)).toEqual({
      type: "success",
      result: {
        terms: [
          { 
            type: "token",
            word: "abc"
          },
          { 
            type: "token",
            word: "and"
          },
          { 
            type: "token",
            word: "def"
          }
        ]
      }
    })
  });

  test("only tokens with unescaped special symbols", () => {
    const query = "abc ? and $ def"
    expect(parseStructuralQuery(query)).toEqual({
      type: "success",
      result: {
        terms: [
          { 
            type: "token",
            word: "abc"
          },
          { 
            type: "token",
            word: "?"
          },
          { 
            type: "token",
            word: "and"
          },
          { 
            type: "token",
            word: "$"
          },
          { 
            type: "token",
            word: "def"
          }
        ]
      }
    })
  });

  test("only tokens with word being split into multiple tokens", () => {
    const query = "it's ? and $ don't"
    expect(parseStructuralQuery(query)).toEqual({
      type: "success",
      result: {
        terms: [
          { 
            type: "token",
            word: "it"
          },
          { 
            type: "token",
            word: "'s"
          },
          { 
            type: "token",
            word: "?"
          },
          { 
            type: "token",
            word: "and"
          },
          { 
            type: "token",
            word: "$"
          },
          { 
            type: "token",
            word: "don"
          },
          { 
            type: "token",
            word: "'t"
          }
        ]
      }
    })
  });

  test("single anchor no constraints", () => {
    const query = "abc $and def"
    expect(parseStructuralQuery(query)).toEqual({
      type: "success",
      result: {
        terms: [
          { 
            type: "token",
            word: "abc"
          },
          { 
            type: "anchor",
            word: "and",
            constraints: []
          },
          { 
            type: "token",
            word: "def"
          }
        ]
      }
    })
  });

  test("single anchor one explicit constraints", () => {
    const query = "abc $[e]and def"
    expect(parseStructuralQuery(query)).toEqual({
      type: "success",
      result: {
        terms: [
          { 
            type: "token",
            word: "abc"
          },
          { 
            type: "anchor",
            word: "and",
            constraints: [
              {
                type: "entity",
                alternatives: []
              }             
            ]
          },
          { 
            type: "token",
            word: "def"
          }
        ]
      }
    })
  });

test("single anchor complex constraints", () => {
    const query = "abc $[e=LOC|CITY&word={city_names}]and def"
    expect(parseStructuralQuery(query)).toEqual({
      type: "success",
      result: {
        terms: [
          { 
            type: "token",
            word: "abc"
          },
          { 
            type: "anchor",
            word: "and",
            constraints: [
              {
                type: "entity",
                alternatives: [
                  {type: "literal", value: "LOC"},
                  {type: "literal", value: "CITY"}
                ]
              },
              {
                type: "word",
                alternatives: [
                  {type: "list", name: "city_names"},
                ]
              }
            ]
          },
          { 
            type: "token",
            word: "def"
          }
        ]
      }
    })
  });

test("simple capture", () => {
    const query = "abc :and def"
    expect(parseStructuralQuery(query)).toEqual({
      type: "success",
      result: {
        terms: [
          { 
            type: "token",
            word: "abc"
          },
          { 
            type: "capture",
            word: "and",
            constraints: []
          },
          { 
            type: "token",
            word: "def"
          }
        ]
      }
    })
  });

  test("simple capture with default expansion but no name", () => {
    const query = "abc <>:and def"
    expect(parseStructuralQuery(query)).toEqual({
      type: "success",
      result: {
        terms: [
          { 
            type: "token",
            word: "abc"
          },
          { 
            type: "capture",
            word: "and",
            constraints: [],
            expand: ""
          },
          { 
            type: "token",
            word: "def"
          }
        ]
      }
    })
  });

test("simple capture with named expansion but no name", () => {
    const query = "abc <U1>:and def"
    expect(parseStructuralQuery(query)).toEqual({
      type: "success",
      result: {
        terms: [
          { 
            type: "token",
            word: "abc"
          },
          { 
            type: "capture",
            word: "and",
            constraints: [],
            expand: "U1"
          },
          { 
            type: "token",
            word: "def"
          }
        ]
      }
    })
  });

test("simple capture with name", () => {
    const query = "abc cap_1:and def"
    expect(parseStructuralQuery(query)).toEqual({
      type: "success",
      result: {
        terms: [
          { 
            type: "token",
            word: "abc"
          },
          { 
            type: "capture",
            word: "and",
            constraints: [],
            name: "cap_1"
          },
          { 
            type: "token",
            word: "def"
          }
        ]
      }
    })
  });

  test("simple capture with name and expansion", () => {
    const query = "abc <U1>cap_1:and def"
    expect(parseStructuralQuery(query)).toEqual({
      type: "success",
      result: {
        terms: [
          { 
            type: "token",
            word: "abc"
          },
          { 
            type: "capture",
            word: "and",
            constraints: [],
            expand: "U1",
            name: "cap_1"
          },
          { 
            type: "token",
            word: "def"
          }
        ]
      }
    })
  });

test("complex capture", () => {
    const query = "abc <U2>cap_1:[w&e=PERSON|{my_list}|`some long value`]and def"
    expect(parseStructuralQuery(query)).toEqual({
      type: "success",
      result: {
        terms: [
          { 
            type: "token",
            word: "abc"
          },
          { 
            type: "capture",
            word: "and",
            constraints: [
              {
                type: "word",
                alternatives: [],
              },
              {
                type: "entity",
                alternatives: [
                  {type: "literal", value:"PERSON"},
                  {type: "list", name:"my_list"},
                  {type: "literal", value:"some long value"},
                ],
              }
            ],
            name: "cap_1",
            expand: "U2"
          },
          { 
            type: "token",
            word: "def"
          }
        ]
      }
    })
  });

  test("failure", () => {
    const query = "abc U2>cap_1:[w&e=PERSON|{my_list}|`some long value`]and def"
    const p = parseStructuralQuery(query)
    expect(p.type == "failure" && p.offset === 6).toBeTrue
  });

});