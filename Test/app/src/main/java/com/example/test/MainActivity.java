package com.example.test;

import android.os.Bundle;

import androidx.appcompat.app.AppCompatActivity;

import java.util.ArrayList;
import java.util.List;

public class MainActivity extends AppCompatActivity {

    UserRepository userRepository;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        MainApplication appState = ((MainApplication)this.getApplication());
        userRepository = appState.getUserRepository();

        List<Test> testList = new ArrayList<>();
        testList.add(new Test("test1", 0));
        testList.add(new Test("test2", 0));

        //userRepository.insert(new User("test2", "0", testList));
        //userRepository.update(new User("test", "1", testList));
        System.out.println(userRepository.findUser("test").size());
    }

}