module PartySpec where

import Test.Hspec
import HomeworkEight.Party
import HomeworkEight.Employee

spec :: Spec
spec = do
    describe "glCons" $
        it "returns a GuestList with two employees and 11 fun" $
            glCons (Emp "Esteban Ibarra" 6) 
            (GL [Emp "Jaime Ibarra" 5] 5)
            `shouldBe`
            GL [Emp "Esteban Ibarra" 6,Emp "Jaime Ibarra" 5] 11
    describe "moreFun" $ do
        it "returns the first GuestList" $
            moreFun (GL [Emp "Esteban Ibarra" 20,Emp "Jaime Ibarra" 5] 25)
            (GL [Emp "Bladimir Ibarra" 6,Emp "Cristina Ibarra" 5] 11)
            `shouldBe`
            GL [Emp "Esteban Ibarra" 20,Emp "Jaime Ibarra" 5] 25
        it "returns the second GuestList" $
            moreFun (GL [Emp "Esteban Ibarra" 20,Emp "Jaime Ibarra" 5] 25)
            (GL [Emp "Bladimir Ibarra" 20,Emp "Cristina Ibarra" 20] 40)
            `shouldBe`
            GL [Emp "Bladimir Ibarra" 20,Emp "Cristina Ibarra" 20] 40
    describe "nextLevel" $ do
        it "returns a GuestList only with the boss" $
            nextLevel (Emp "Bob" 10) [] `shouldBe` (GL [Emp "Bob" 10] 10, mempty)
        it "returns two GuestLists, one with the boss and another one w/o him" $
            nextLevel (Emp "Bob" 10) [(GL [Emp "Joe" 15, Emp "Trevor" 20] 35
                                      ,GL [Emp "Ana" 10, Emp "Joel" 20] 30)]
            `shouldBe`
            (GL [Emp "Bob" 10,Emp "Ana" 10, Emp "Joel" 20] 40
            ,GL [Emp "Joe" 15,Emp "Trevor" 20,Emp "Ana" 10, Emp "Joel" 20] 65) 
    describe "maxFun" $
        it "returns a GuestList with all the employees in a tree" $
            maxFun testCompany `shouldBe`
            GL [Emp "Stan" 9,Emp "Joe" 5,Emp "Fred" 3,Emp "John" 1,
                Emp "Sue" 5,Emp "Sam" 4] 27
             
    