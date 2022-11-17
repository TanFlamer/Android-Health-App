package com.example.test;

import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModel;
import androidx.lifecycle.ViewModelProvider;

import com.example.test.databaseFiles.entity.User;
import com.example.test.databaseFiles.viewModal.UserViewModal;

import java.util.ArrayList;
import java.util.List;

public class MainActivity extends AppCompatActivity {

    private EditText courseNameEdt, courseDescEdt, courseDurationEdt;
    private Button courseBtn;
    private UserViewModal userViewModal;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        courseNameEdt = findViewById(R.id.idEdtCourseName);
        courseDescEdt = findViewById(R.id.idEdtCourseDescription);
        courseDurationEdt = findViewById(R.id.idEdtCourseDuration);
        courseBtn = findViewById(R.id.idBtnSaveCourse);

        userViewModal = new ViewModelProvider(this).get(UserViewModal.class);
        userViewModal.getAllUsers().observe(this, users -> Toast.makeText(this, "Dataset changed", Toast.LENGTH_SHORT).show());

        courseBtn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Toast.makeText(getApplicationContext(), "test", Toast.LENGTH_SHORT).show();
                //userViewModal.insert(new User());
            }
        });

    }
}