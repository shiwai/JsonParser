module JsonTest
    open Json
    open FsUnit
    open NUnit.Framework
    open System.Text.RegularExpressions

    [<TestFixture>]
    type ``DoTest`` ()=

        [<Test>]
        member x.ParseJsonTest () = 
            "{
                \"x\" : 100
            }" |> Parse |> Option.get |> should equal (json.Object([("x", Json.Number(100.0))]))

        [<Test>]
        member x.ParseJsonTest2 () = 
            "
            {
                \"key\" : \"va,lue\",
                \"monaka\" : true
            }
            " |> Parse |> Option.get |> should equal (
                    Json.Object(
                        [
                            ("key", String("va,lue")); 
                            ("monaka", Bool(true))
                        ]
                        ))

        [<Test>]
        member x.ParseJsonTest3 () =
            "
                [ \"aaa\", 10, {\"name\" : \"cat\"} ]
                
            " |> Parse |> Option.get |> should equal (
                Json.Array(
                    [
                        String("aaa"); 
                        Number(10.0); 
                        Object([("name", String("cat"))])
                    ]
                )
            )

        (* todo: 期待値をちゃんと書く *)
        [<Test>]
        member x.ParseJsonTest4() = 
            """{"coord":{"lon":139.81,"lat":35.68},"sys":{"type":1,"id":7619,"message":0.0371,"country":"Japan","sunrise":1408219280,"sunset":1408267698},"weather":[{"id":701,"main":"Mist","description":"mist","icon":"50d"}],"base":"cmc stations","main":{"temp":297.51,"pressure":1013,"humidity":83,"temp_min":296.15,"temp_max":299.15},"wind":{"speed":1.5,"deg":20},"clouds":{"all":75},"dt":1408238160,"id":1865485,"name":"Tokyo","cod":200}"""
            |> Parse |> Option.get |> should equal (
                Json.Object([("coord", Object(
                                          [("lon", Number(139.81)); ("lat", String("35.68"))]
                                        ));
                                        ("sys", Object(
                                          [
                                          ("type", Number(1.0));
                                          ("id", Number(7619.0));
                                          ("message", Number(0.0371));
                                          ("country", String("japan"));
                                          ("sunrise", Number(1408219280.0));
                                          ("sunset",  Number(1408267698.0));
                                          ]
                                        ))
                                        ]))
            

