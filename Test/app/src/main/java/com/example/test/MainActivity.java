package com.example.test;

import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.test.databaseFiles.entity.User;
import com.example.test.databaseFiles.viewModal.UserViewModal;

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
        userViewModal.getAllUsers().observe(this, users -> Toast.makeText(this, String.valueOf(users.size()), Toast.LENGTH_SHORT).show());

        courseBtn.setOnClickListener(view -> {
            userViewModal.delete(new User(Integer.valueOf(courseDurationEdt.getText().toString()), courseNameEdt.getText().toString(), courseDescEdt.getText().toString()));
            courseNameEdt.getText().clear();
            courseDurationEdt.getText().clear();
            courseDescEdt.getText().clear();
        });

    }
}